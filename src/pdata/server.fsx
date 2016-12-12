#if INTERACTIVE
#r "System.Xml.Linq.dll"
#I "../../packages"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Suave/lib/net40/Suave.dll"
#else
module Services.PivotData
#endif
#nowarn "1104"
open System
open System.IO
open FSharp.Data

// ----------------------------------------------------------------------------
// Types for values and transformations
// ----------------------------------------------------------------------------

type Value = 
  | String of string
  | Number of decimal

type Aggregation = 
  | GroupKey
  | CountAll
  | CountDistinct of string
  | ReturnUnique of string
  | ConcatValues of string
  | Sum of string
  | Mean of string

type SortDirection =
  | Ascending
  | Descending 

type Paging =
  | Take of int
  | Skip of int
  
type Transformation = 
  | DropColumns of string list
  | SortBy of (string * SortDirection) list
  | GroupBy of string list * Aggregation list
  | FilterBy of (string * bool * string) list
  | Paging of Paging list
  | GetSeries of string * string
  | Empty

module Transform = 

  let private chunkBy f s = seq {
    let mutable acc = []
    for v in s do 
      if f v then
        yield List.rev acc
        acc <- []
      else acc <- v::acc
    yield List.rev acc }

  let rec private parseAggs acc = function 
    | "key"::rest -> parseAggs (GroupKey::acc) rest
    | "count-all"::rest -> parseAggs (CountAll::acc) rest
    | "count-dist"::fld::rest -> parseAggs (CountDistinct(fld)::acc) rest
    | "unique"::fld::rest -> parseAggs (ReturnUnique(fld)::acc) rest
    | "concat-vals"::fld::rest -> parseAggs (ConcatValues(fld)::acc) rest
    | "sum"::fld::rest -> parseAggs (Sum(fld)::acc) rest
    | "mean"::fld::rest -> parseAggs (Mean(fld)::acc) rest
    | [] -> List.rev acc
    | aggs -> failwith (sprintf "Invalid aggregation operation: %s" (String.concat "/" aggs))

  let private parseChunk = function
    | "drop"::columns -> DropColumns(columns)
    | "sort"::columns -> SortBy(columns |> List.chunkBySize 2 |> List.map (function [f; "asc"] -> f, Ascending | [f; "desc"] -> f, Descending | _ -> failwith "Invalid sort by order"))
    | "filter"::conds ->
        conds |> List.chunkBySize 3 |> List.map (function 
          | [f; "eq"; v] -> f, true, v
          | [f; "neq"; v] -> f, false, v
          | _ -> failwith "Invalid filter condition") |> FilterBy
    | "group"::args -> 
        let fields, aggs = args |> List.partition (fun s -> s.StartsWith("by-"))
        let fields = fields |> List.map (fun s -> s.Substring(3))
        GroupBy(fields, parseAggs [] aggs)
    | "page"::ops -> 
        let ops = ops |> List.chunkBySize 2 |> List.map (function
          | ["take"; n ] -> Take(int n)
          | ["skip"; n ] -> Skip(int n)
          | _ -> failwith "Invalid paging operation")
        Paging(ops)
    | "series"::k::v::[] -> GetSeries(k, v)
    | [] -> Empty
    | ch -> failwith (sprintf "Not a valid transformation: %s" (String.concat "/" ch))

  let fromUrl (s:string) = 
    s.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries) 
    |> Array.map System.Web.HttpUtility.UrlDecode
    |> chunkBy ((=) "then")
    |> List.ofSeq
    |> List.map parseChunk 
    |> List.rev

// ----------------------------------------------------------------------------
// Evaluate query
// ----------------------------------------------------------------------------

let inline pickField name obj = 
  Array.pick (fun (n, v) -> if n = name then Some v else None) obj

let inline the s = match List.ofSeq s with [v] -> v | _ -> failwith "Not unique"
let asString = function String s -> s | Number n -> string n
let asDecimal = function String s -> decimal s | Number n -> n

let applyAggregation kvals group = function
 | GroupKey -> kvals
 | CountAll -> [ "count", Number(group |> Seq.length |> decimal) ]
 | CountDistinct(fld) -> [ fld, Number(group |> Seq.distinctBy (pickField fld) |> Seq.length |> decimal) ]
 | ReturnUnique(fld) -> [ fld, group |> Seq.map (pickField fld) |> the ]
 | ConcatValues(fld) -> [ fld, group |> Seq.map(fun obj -> pickField fld obj |> asString) |> Seq.distinct |> String.concat ", " |> String ]
 | Sum(fld) -> [ fld, group |> Seq.sumBy (fun obj -> pickField fld obj |> asDecimal) |> Number ]
 | Mean(fld) -> [ fld, group |> Seq.averageBy (fun obj -> pickField fld obj |> asDecimal) |> Number ]

let compareFields o1 o2 (fld, order) = 
  let reverse = if order = Descending then -1 else 1
  match pickField fld o1, pickField fld o2 with
  | Number d1, Number d2 -> reverse * compare d1 d2
  | String s1, String s2 -> reverse * compare s1 s2
  | _ -> failwith "Cannot compare values"

let transformData (objs:seq<(string * Value)[]>) = function
  | Empty -> objs
  | GetSeries(k, v) ->
      objs |> Seq.map (fun obj ->
        let kn, kval = Array.find (fst >> (=) k) obj
        let vn, vval = Array.find (fst >> (=) v) obj
        [| kn, kval; vn, vval |])
  | Paging(pgops) ->
      pgops |> Seq.fold (fun objs -> function
        | Take n -> objs |> Seq.truncate n
        | Skip n -> objs |> Seq.skip n) objs
  | DropColumns(flds) ->
      let dropped = set flds
      objs |> Seq.map (fun obj ->
        obj |> Array.filter (fst >> dropped.Contains >> not))
  | SortBy(flds) ->
      let flds = List.rev flds
      objs |> Seq.sortWith (fun o1 o2 ->
        let optRes = flds |> List.map (compareFields o1 o2) |> List.skipWhile ((=) 0) |> List.tryHead
        defaultArg optRes 0)
  | FilterBy(conds) ->
      conds |> List.fold (fun objs (fld, eq, value) ->
        objs |> Seq.filter (fun o -> 
          match pickField fld o with
          | String v -> v = value
          | Number n -> n = decimal value)) objs
  | GroupBy(flds, aggs) ->
      let aggs = List.rev aggs
      objs 
      |> Seq.groupBy (fun j -> List.map (fun f -> pickField f j) flds)
      |> Seq.map (fun (kvals, group) ->
        aggs 
        |> List.collect (applyAggregation (List.zip flds kvals) group)
        |> Array.ofSeq)


// ----------------------------------------------------------------------------
// Loading data for olympics 
// ----------------------------------------------------------------------------

module Olympics = 
  let dataRoot = 
    if System.Reflection.Assembly.GetExecutingAssembly().IsDynamic then 
      __SOURCE_DIRECTORY__ + "/../../data"
    else IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location) + "/../data"

  let [<Literal>] Root = __SOURCE_DIRECTORY__ + "/../../data/medals-expanded.csv"
  type Medals = CsvProvider<Root, Schema="Gold=int, Silver=int, Bronze=int", CacheRows=false>
  let medals = Medals.Load(dataRoot + "/medals-expanded.csv")
              
  // http://www.topendsports.com/events/summer/countries/country-codes.htm
  type Codes = FSharp.Data.HtmlProvider<const(__SOURCE_DIRECTORY__ + "/../../data/countrycodes.html")>
  let countries = 
    [ yield "KOS", "Kosovo"
      yield "SRB", "Serbia"
      yield "TTO", "Trinidad and Tobago"
      for r in Codes.Parse(File.ReadAllText(dataRoot + "/countrycodes.html")).Tables.``3-Digit Country Codes``.Rows do 
        yield r.Code, r.Country.TrimEnd('*') ] |> dict

  let allData = Lazy.Create(fun () ->
    medals.Rows |> Seq.map (fun r ->
      [| "Games", String r.Games; "Year", Number (decimal r.Year);  "Sport", String r.Sport
         "Discipline", String r.Discipline; "Athlete", String r.Athlete; "Team", String(countries.[r.Team]);
         "Gender", String r.Gender; "Event", String r.Event; "Medal", String r.Medal
         "Gold", Number (decimal r.Gold); "Silver", Number (decimal r.Silver); 
         "Bronze", Number (decimal r.Bronze) |]) |> Array.ofSeq )

  let metadata = 
    [ "Games", "string"; "Year", "number"; "Sport", "string"; "Discipline", "string"; 
      "Athlete", "string"; "Team", "string"; "Gender", "string"; "Event", "string"; 
      "Medal", "string"; "Gold", "number"; "Silver", "number"; "Bronze", "number" ]

// ----------------------------------------------------------------------------
// Loading data for smlouvy
// ----------------------------------------------------------------------------

module Smlouvy = 
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

  type Index = XmlProvider<const(__SOURCE_DIRECTORY__ + "/../smlouvy/index.xml")>
  type Dump = XmlProvider<const(__SOURCE_DIRECTORY__ + "/../smlouvy/smlouva.xml")>

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

        [|"Uzavřeno", z.Smlouva.DatumUzavreni.ToString("yyyy-MM-dd") |> String
          "Publikováno", z.CasZverejneni.ToString("yyyy-MM-dd") |> String
          "Hodnota", hodnota |> Number
          "Chybí hodnota", String(if hodnotaChybi then "chybí" else "zadána")
          "Subjekt", z.Smlouva.Subjekt.Nazev |> String
          "Útvar", String(defaultArg z.Smlouva.Subjekt.Utvar "")
          "Schválil", String(defaultArg z.Smlouva.Schvalil "Nezadáno")
          "Předmět", z.Smlouva.Predmet |> String
          "Odkaz", z.Odkaz |> String
          "Platnost", String(if z.PlatnyZaznam then "platné" else "neplatné")
          "Příjemci", prijemci |> String
          "Příjemci (IČO)", prijemciIco |> String |] |> smlouvy.Add
    smlouvy.ToArray()

  let allData = Lazy.Create(fun () ->
    updateCache() |> Async.RunSynchronously
    readFiles() )

  let metadata = 
    [ "Uzavřeno", "string"; "Publikováno", "string"; "Hodnota", "number"
      "Chybí hodnota", "string"; "Subjekt", "string"; "Útvar", "string"
      "Schválil", "string"; "Předmět", "string"; "Odkaz", "string"
      "Platnost", "string"; "Příjemci", "string"; "Příjemci (IČO)", "string" ]            


// ----------------------------------------------------------------------------
// Create faceted service
// ----------------------------------------------------------------------------

open Suave
open Suave.Filters

let serializeValue = function String s -> JsonValue.String s | Number n -> JsonValue.Number n

let serialize isPreview isSeries data = 
  let data = if isPreview then Array.truncate 10 data else data
  data 
  |> Array.map (fun (fields:_[]) ->
    if isSeries then
      JsonValue.Array [| serializeValue (snd fields.[0]); serializeValue (snd fields.[1]) |]
    else 
      fields |> Array.map (fun (k, v) -> k, serializeValue v) |> JsonValue.Record)
  |> JsonValue.Array

let app = 
  choose [
    pathScan "/%s/metadata" (fun source ->
      let metadata = 
        if source = "olympics" then Olympics.metadata
        elif source = "smlouvy" then Smlouvy.metadata
        else failwith "Unknown source"
      let json = JsonValue.Record [| for k, v in metadata -> k, JsonValue.String v |]
      Successful.OK(json.ToString()) )

    pathScan "/%s/%s" (fun (source, _) -> request(fun r -> 
      let isPreview = r.query |> List.exists (fun (k, _) -> k = "preview")
      let source = 
        if source = "olympics" then Olympics.allData.Value
        elif source = "smlouvy" then Smlouvy.allData.Value
        else failwith "Unknown source"
      choose [
        pathScan "/%s/%s/%s" (fun (_, dataOrRange, tfs) ->
          let tfs = Transform.fromUrl tfs
          let res = tfs |> List.fold transformData (Seq.ofArray source) |> Array.ofSeq
          if dataOrRange = "data" then
            let isSeries = match List.last tfs with GetSeries _ -> true | _ -> false
            Successful.OK ((serialize isPreview isSeries res).ToString()) 
          elif dataOrRange = "range" then
            let fld = System.Web.HttpUtility.UrlDecode(r.rawQuery)
            printfn "Field: %s" fld
            let json = res |> Array.map (pickField fld) |> Array.distinct |> Array.map serializeValue |> JsonValue.Array
            Successful.OK (json.ToString())
          else 
            RequestErrors.BAD_REQUEST "expected data or range request" ) ]
    )) ]