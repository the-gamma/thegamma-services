#if INTERACTIVE
#I "../../packages"
#r "System.Xml.Linq.dll"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "Suave/lib/net40/Suave.dll"
#load "../serializer.fs"
#load "../facet.fs"
#else
module Services.Smlouvy
#endif
#nowarn "1104"
open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Services.Facets

// --------------------------------------------------------------------------------------
// Loading and pre-processing data
// --------------------------------------------------------------------------------------

let cache = 
  if System.Reflection.Assembly.GetExecutingAssembly().IsDynamic then 
    __SOURCE_DIRECTORY__ + "/../../cache"
  else IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location) + "/../cache"
if not (Directory.Exists cache) then Directory.CreateDirectory cache |> ignore
let (</>) a b = Path.Combine(a, b)

type Record = 
  { DatumUzavreni : System.DateTime
    DatumPublikace : System.DateTime
    Hodnota : decimal
    ChybiHodnota : string   
    SubjektNazev : string
    SubjektUtvar : string
    Schavlil : string
    Predmet : string
    Odkaz : string
    Platne : string
    Prijemci : string
    PrijemciIco : string }

type Index = XmlProvider<const(__SOURCE_DIRECTORY__ + "/index.xml")>
type Dump = XmlProvider<const(__SOURCE_DIRECTORY__ + "/smlouva.xml")>

let updateCache() = async {
  use wc = new System.Net.WebClient()
  let! index = Index.AsyncLoad("https://data.smlouvy.gov.cz")
  for m in index.Dumps do
    let fn suffix = sprintf "%04d-%02d%s.xml" m.Rok m.Mesic suffix
    if File.Exists(cache </> fn "-partial") then File.Delete(fn "-partial")
    if not (File.Exists(cache </> fn "")) then 
      let target = cache </> fn (if m.DokoncenyMesic then "" else "-partial")
      do! wc.AsyncDownloadFile(System.Uri(m.Odkaz), target) }

let readFiles () = 
  let smlouvy = ResizeArray<_>(10000)
  for f in Directory.GetFiles(cache) do
    let dump = Dump.Load(f)
    for z in dump.Zaznams do
      let prijemci = 
        [ for s in z.Smlouva.SmluvniStranas do
            if s.Prijemce = Some 1 then yield s.Nazev ]
      let prijemciIco = 
        [ for s in z.Smlouva.SmluvniStranas do
            if s.Prijemce = Some 1 && s.Ico <> None then yield s.Ico.Value.String.Value ]
      let prijemci = if prijemci.IsEmpty then "Nezadáno" else String.concat ", " prijemci
      let prijemciIco = if prijemciIco.IsEmpty then "Nezadáno" else String.concat ", " prijemciIco

      let hodnota, hodnotaChybi = 
        match z.Smlouva.HodnotaBezDph, z.Smlouva.HodnotaVcetneDph with
        | _, Some h when h <> 0.0M -> h, false
        | Some h, _ when h <> 0.0M -> h * 1.2M, false
        | _ -> 0.0M, true

      { DatumPublikace = z.CasZverejneni
        DatumUzavreni = z.Smlouva.DatumUzavreni
        Hodnota = hodnota
        ChybiHodnota = if hodnotaChybi then "chybí" else "zadána"
        SubjektNazev = z.Smlouva.Subjekt.Nazev
        SubjektUtvar = defaultArg z.Smlouva.Subjekt.Utvar ""
        Schavlil = defaultArg z.Smlouva.Schvalil "Nezadáno"
        Predmet = z.Smlouva.Predmet
        Odkaz = z.Odkaz
        Platne = if z.PlatnyZaznam then "platné" else "neplatné"
        Prijemci = prijemci
        PrijemciIco = prijemciIco } |> smlouvy.Add
  smlouvy.ToArray()

let smlouvy = Lazy.Create (fun () ->
  updateCache() |> Async.RunSynchronously
  readFiles() )

// --------------------------------------------------------------------------------------
// Faceted data service
// --------------------------------------------------------------------------------------


let facets : Lazy<list<string * Facet<Record>>> = Lazy.Create (fun () ->
  let validYears = smlouvy.Value |> Seq.map (fun s -> s.DatumUzavreni.Year) |> Seq.distinct |> Array.ofSeq
  let pubYears = smlouvy.Value |> Seq.map (fun s -> s.DatumPublikace.Year) |> Seq.distinct |> Array.ofSeq
  let months = System.Globalization.CultureInfo.GetCultureInfo("cs-CZ").DateTimeFormat.MonthNames |> Array.filter ((<>) "")
  [ // Single-choice 
    yield "příjemce", Filter("příjemce", false, fun r -> Some(r.Prijemci, makeThingSchema "Organization" r.Prijemci))
    yield "platnost", Filter("platnost", false, fun r -> Some(r.Platne, noSchema))
    yield "schválil", Filter("schválil", false, fun r -> Some(r.Schavlil, makeThingSchema "Person" r.Schavlil))
    yield "subjekt", Filter("subjekt", false, fun r -> Some(r.SubjektNazev, makeThingSchema "Organization" r.SubjektNazev))
    yield "hodnota vyplněna", Filter("hodnota vyplněna", false, fun r -> Some(r.ChybiHodnota, noSchema))

    // Multi-choice
    yield "příjemci", Filter("příjemci", true, fun r -> Some(r.Prijemci, makeThingSchema "Organization" r.Prijemci))
    yield "schválili", Filter("schválili", true, fun r -> Some(r.Schavlil, makeThingSchema "Person" r.Schavlil))
    yield "subjekty", Filter("subjekty", true, fun r -> Some(r.SubjektNazev, makeThingSchema "Organization" r.SubjektNazev))

    // Multi-level facet for selecting date/month/day 
    let monthSelect years (f:Record -> DateTime) =  
      [ for y in years ->
          sprintf "year-%d" y, string y, makeThingSchema "Date" (string y), Filter("měsíc" + string y, false, fun (r:Record) -> 
            let m = months.[(f r).Month - 1]
            if (f r).Year = y then Some(m, makeThingSchema "Date" m) else None) ]
    let daySelect years (f:Record -> DateTime) =  
      [ for y in years ->
          sprintf "year-%d" y, string y, makeThingSchema "Date" (string y), Choice("měsíc", 
            [ for i, m in Seq.mapi (fun i m -> i+1, m) months ->
              sprintf "month-%d-%d" y i, m, makeThingSchema "Date" m, Filter("den" + string y + "-" + string i, false, fun (r:Record) -> 
                let d = (f r).Day |> string
                if (f r).Year = y && (f r).Month = i then Some(d, makeThingSchema "Date" d) else None) ]) ]    
    yield "měsíc publikace", Choice("měsíc publikce", monthSelect pubYears (fun r -> r.DatumPublikace))
    yield "měsíc uzavření", Choice("měsíc uzavření", monthSelect pubYears (fun r -> r.DatumUzavreni))
    yield "den publikace", Choice("den publikce", daySelect pubYears (fun r -> r.DatumPublikace))
    yield "den uzavření", Choice("den uzavření", daySelect pubYears (fun r -> r.DatumUzavreni)) ] )

// ----------------------------------------------------------------------------
// Create faceted service
// ----------------------------------------------------------------------------

open Suave

let app = request (fun r ->
  createFacetApp smlouvy.Value
    [|  ("Uzavřeno", "string", fun r -> r.DatumUzavreni.ToString("yyyy-MM-dd") |> box) 
        ("Publikováno", "string", fun r -> r.DatumPublikace.ToString("yyyy-MM-dd") |> box) 
        ("Hodnota", "float", fun r -> r.Hodnota |> float |> box) 
        ("Chybí hodnota", "string", fun r -> r.ChybiHodnota |> box) 
        ("Subjekt", "string", fun r -> r.SubjektNazev |> box) 
        ("Útvar", "string", fun r -> r.SubjektUtvar |> box) 
        ("Schválil", "string", fun r -> r.Schavlil |> box) 
        ("Předmět", "string", fun r -> r.Predmet |> box) 
        ("Odkaz", "string", fun r -> r.Odkaz |> box) 
        ("Platnost", "string", fun r -> r.Platne |> box) 
        ("Příjemci", "string", fun r -> r.Prijemci |> box) 
        ("Příjemci (IČO)", "string", fun r -> r.PrijemciIco |> box)  |]
    facets.Value)
