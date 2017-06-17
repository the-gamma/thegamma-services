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

// ------------------------------------------------------------------------------------------------
// Types for values and transformations
// ------------------------------------------------------------------------------------------------

type Value = 
  | Bool of bool
  | String of string
  | Number of float
  | Date of DateTimeOffset

[<RequireQualifiedAccess>]
type GroupAggregation = 
  | GroupKey
  | CountAll
  | CountDistinct of string
  | ConcatValues of string
  | Sum of string
  | Mean of string

[<RequireQualifiedAccess>]
type WindowAggregation = 
  | Min of string
  | Max of string
  | Mean of string
  | Sum of string
  | FirstKey
  | LastKey
  | MiddleKey

type SortDirection =
  | Ascending
  | Descending 

type Paging =
  | Take of int
  | Skip of int
  
type FilterOperator = 
  | And | Or

type RelationalOperator = 
  | Equals 
  | NotEquals 
  | LessThan
  | GreaterThan 
  | InRange
  | Like

type FilterCondition = RelationalOperator * string * string

type Transformation = 
  | DropColumns of string list
  | SortBy of (string * SortDirection) list
  | GroupBy of string list * GroupAggregation list
  | WindowBy of string * int * WindowAggregation list
  | ExpandBy of string * WindowAggregation list
  | FilterBy of FilterOperator * FilterCondition list
  | Paging of Paging list
  | Empty

type Action = 
  | Metadata
  | GetSeries of string * string
  | GetTheData 
  | GetRange of string

type Query = 
  { Transformations : Transformation list
    Action : Action }


// ------------------------------------------------------------------------------------------------
// Parsing transformations
// ------------------------------------------------------------------------------------------------

module Transform = 

  let groupUnaryOps = 
    [ "count-dist", GroupAggregation.CountDistinct; "mean", GroupAggregation.Mean
      "concat-vals", GroupAggregation.ConcatValues; "sum", GroupAggregation.Sum ]
  let groupNullaryOps =
    [ "key", GroupAggregation.GroupKey; "count-all", GroupAggregation.CountAll ]

  let winUnaryOps = 
    [ "mean", WindowAggregation.Mean; "sum", WindowAggregation.Sum
      "min", WindowAggregation.Min; "max", WindowAggregation.Max ]
  let winNullaryOps = 
    [ "first-key", WindowAggregation.FirstKey; "last-key", WindowAggregation.LastKey
      "mid-key", WindowAggregation.MiddleKey ]

  let trimIdent (s:string) = 
    if s.StartsWith("'") && s.EndsWith("'") then s.Substring(1, s.Length-2)
    else s

  let parseOp nullary unary (op:string) =
    match nullary |> List.tryFind (fun (k, f) -> k = op) with
    | Some(_, op) -> op
    | _ ->
      let parsed = unary |> List.tryPick (fun (k, f) ->
        if op.StartsWith(k) then Some(f(trimIdent(op.Substring(k.Length + 1))))
        else None)
      if parsed.IsSome then parsed.Value else failwith "Unknonw operation"

  let parseAction (op, args) = 
    match op, args with
    | "metadata", [] -> Metadata, true
    | "series", [k; v] -> GetSeries(k, v), true
    | "range", [f] -> GetRange(f), true
    | _ -> GetTheData, false

  let operators = 
    [ Equals, " eq "; NotEquals, " neq "; LessThan, " lte "; GreaterThan, " gte "; InRange, " in "; Like, " like " ]

  let parseCondition (cond:string) = 
    let cond = cond.Trim()
    let start = if cond.StartsWith("'") then cond.IndexOf('\'', 1) else 0
    let optRes = 
      operators |> Seq.tryPick (fun (op, s) ->
        let i = cond.IndexOf(s)
        if i = -1 then None else
          Some(op, trimIdent (cond.Substring(0, i)), trimIdent (cond.Substring(i + s.Length))))
    match optRes with
    | None -> failwithf "Incorrectly formatted condition: >>%s<<" cond
    | Some res -> res

  let parseTransform (op, args) = 
    match op, args with
    | "drop", columns -> DropColumns(List.map trimIdent columns)
    | "sort", columns -> SortBy(columns |> List.map (fun col -> 
        if col.EndsWith(" asc") then trimIdent (col.Substring(0, col.Length-4)), Ascending
        elif col.EndsWith(" desc") then trimIdent (col.Substring(0, col.Length-5)), Descending
        else trimIdent col, Ascending))
    | "filter", "and"::conds -> FilterBy(And, List.map parseCondition conds)
    | "filter", "or"::conds -> FilterBy(Or, List.map parseCondition conds)
    | "filter", conds -> FilterBy(And, List.map parseCondition conds)
    | "groupby", ops ->
        let keys = ops |> List.takeWhile (fun s -> s.StartsWith "by ") |> List.map (fun s -> trimIdent (s.Substring(3)))
        let aggs = ops |> List.skipWhile (fun s -> s.StartsWith "by ") |> List.map (parseOp groupNullaryOps groupUnaryOps)
        GroupBy(keys, aggs)
    | "windowby", key::size::ops ->
        let key = trimIdent (key.Substring(3))
        let aggs = ops |> List.map (parseOp winNullaryOps winUnaryOps)
        WindowBy(key, int size, aggs)
    | "expandby", key::ops ->
        let key = trimIdent (key.Substring(3))
        let aggs = ops |> List.map (parseOp winNullaryOps winUnaryOps)
        ExpandBy(key, aggs)
    | "take", [n] -> Paging [Take (int n)]
    | "skip", [n] -> Paging [Skip (int n)]
    | _ -> failwith "Unsupported transformation"

  let parseArgs (s:string) = 
    let rec loop i quoted current acc = 
      let parseCurrent () = System.String(Array.ofList (List.rev current))
      if i = s.Length then List.rev (parseCurrent()::acc) else
      let c = s.[i]
      if c = '\'' && quoted then loop (i + 1) false (c::current) acc
      elif c = '\'' && not quoted then loop (i + 1) true (c::current) acc
      elif c = ',' && not quoted then loop (i + 1) quoted [] (parseCurrent()::acc)
      else loop (i + 1) quoted (c::current) acc
    loop 0 false [] [] 

  let parseChunk (s:string) =
    let openPar, closePar = s.IndexOf('('), s.LastIndexOf(')')
    if openPar = -1 || closePar = -1 then s, []
    else s.Substring(0, openPar), parseArgs (s.Substring(openPar + 1, closePar - openPar - 1))
    
  let fromUrl (s:string) = 
    let chunks = 
      System.Web.HttpUtility.UrlDecode(s)
        .Split([|'$'|], StringSplitOptions.RemoveEmptyEntries) 
      |> Array.map parseChunk 
    if chunks.Length = 0 then { Transformations = []; Action = GetTheData }
    else
      let action, explicit = parseAction (chunks.[chunks.Length - 1])
      let chunks = List.ofArray (if explicit then chunks.[0 .. chunks.Length - 2] else chunks)
      { Transformations = List.map parseTransform chunks; Action = action }    

// ----------------------------------------------------------------------------
// Evaluate query
// ----------------------------------------------------------------------------

let inline pickField name obj = 
  Array.pick (fun (n, v) -> if n = name then Some v else None) obj

let asString = function String s -> s | Number n -> string n | Date d -> d.ToString("g") | Bool b -> string b
let asFloat = function String s -> float s | Number n -> n | Date d -> float d.Ticks | Bool true -> 1. | Bool false -> 0.

let applyGroupAggregation kvals group = function
 | GroupAggregation.GroupKey -> kvals
 | GroupAggregation.CountAll -> [ "count", Number(group |> Seq.length |> float) ]
 | GroupAggregation.CountDistinct(fld) -> [ fld, Number(group |> Seq.distinctBy (pickField fld) |> Seq.length |> float) ]
 | GroupAggregation.ConcatValues(fld) -> [ fld, group |> Seq.map(fun obj -> pickField fld obj |> asString) |> Seq.distinct |> String.concat ", " |> String ]
 | GroupAggregation.Sum(fld) -> [ fld, group |> Seq.sumBy (fun obj -> pickField fld obj |> asFloat) |> Number ]
 | GroupAggregation.Mean(fld) -> [ fld, group |> Seq.averageBy (fun obj -> pickField fld obj |> asFloat) |> Number ]

let applyWinAggregation kname group agg = 
  let nums fld = group |> Seq.map (fun obj -> pickField fld obj |> asFloat)
  let kvalues = Seq.map (pickField kname) group |> Array.ofSeq
  match agg with
  | WindowAggregation.Mean(fld) -> [ fld, nums fld |> Seq.average |> Number ]
  | WindowAggregation.Min(fld) -> [ fld, nums fld |> Seq.min |> Number ]
  | WindowAggregation.Max(fld) -> [ fld, nums fld |> Seq.max |> Number ]
  | WindowAggregation.Sum(fld) -> [ fld, nums fld |> Seq.sum |> Number ]
  | WindowAggregation.FirstKey -> [ "first " + kname, kvalues.[0] ]
  | WindowAggregation.LastKey -> [ "last " + kname, kvalues.[kvalues.Length-1] ]
  | WindowAggregation.MiddleKey -> [ "middle " + kname, kvalues.[(kvalues.Length-1)/2] ]

let getExpandAggregationFunction kname agg = 
  let ret fld f = fun row -> fld, pickField fld row |> asFloat |> f |> Number
  match agg with
  | WindowAggregation.Mean(fld) -> 
      let mutable sum = 0.
      let mutable count = 0.
      ret fld (fun v -> sum <- sum + v; count <- count + v; sum / count)
  | WindowAggregation.Min(fld) -> 
      let mutable min = Double.MaxValue
      ret fld (fun v -> (if v < min then min <- v); min)
  | WindowAggregation.Max(fld) ->
      let mutable max = Double.MinValue
      ret fld (fun v -> (if v > max then max <- v); max)
  | WindowAggregation.Sum(fld) -> 
      let mutable sum = 0.
      ret fld (fun v -> sum <- sum + v; sum)
  | WindowAggregation.FirstKey -> 
      let mutable first = None
      fun row -> (if first.IsNone then first <- Some (pickField kname row)); "first " + kname, first.Value
  | WindowAggregation.LastKey -> 
      fun row -> "last " + kname, pickField kname row
  | WindowAggregation.MiddleKey -> 
      let keys = ResizeArray<_>()
      fun row -> keys.Add(pickField kname row); "middle " + kname, keys.[keys.Count/2]

let compareFields o1 o2 (fld, order) = 
  let reverse = if order = Descending then -1 else 1
  match pickField fld o1, pickField fld o2 with
  | Number d1, Number d2 -> reverse * compare d1 d2
  | String s1, String s2 -> reverse * compare s1 s2
  | Date d1, Date d2 -> reverse * compare d1 d2
  | _ -> failwith "Cannot compare values"

let evalCondition op actual (expected:string) =
  match op, actual with 
  | Like, String s -> s.ToLower().Contains(expected.ToLower())
  | Like, _ -> failwith "Like can only be used on strings"
  | _, Date _ -> failwith "Comparison on dates not supported"
  | Equals, Bool b -> (expected.ToLower() = "true" && b) || (expected.ToLower() = "false" && not b)
  | NotEquals, Bool b -> (expected.ToLower() = "true" && not b) || (expected.ToLower() = "false" && b)
  | Equals, String s -> expected = s
  | NotEquals, String s -> expected <> s
  | (Equals | NotEquals), Number _ -> failwith "Equals and not equals work only on strings or booleans"
  | GreaterThan, Number n -> n > float expected
  | LessThan, Number n -> n < float expected
  | InRange, Number n -> let expected = expected.Split(',') in n > float expected.[0] && n < float expected.[1]
  | (GreaterThan | LessThan | InRange), (Bool _ | String _) -> failwith "Relational operator work only on numbers"

let transformData (objs:seq<(string * Value)[]>) = function
  | Empty -> objs
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
  | FilterBy(op, conds) ->
      objs |> Seq.filter (fun o ->
        let f = match op with And -> Seq.forall | Or -> Seq.exists
        conds |> f (fun (op, fld, value) -> evalCondition op (pickField fld o) value))
  | WindowBy(fld, size, aggs) ->
      objs 
      |> Seq.sortBy (pickField fld)
      |> Seq.windowed size 
      |> Seq.map (fun win ->
        aggs
        |> List.collect (applyWinAggregation fld win)
        |> Array.ofSeq )
  | ExpandBy(fld, aggs) ->
      let funcs = aggs |> Seq.map (getExpandAggregationFunction fld) |> Array.ofSeq
      objs 
      |> Seq.sortBy (pickField fld)
      |> Seq.map (fun row ->
          funcs |> Array.map (fun f -> f row))
  | GroupBy(flds, aggs) ->
      objs 
      |> Seq.groupBy (fun j -> List.map (fun f -> pickField f j) flds)
      |> Seq.map (fun (kvals, group) ->
        aggs 
        |> List.collect (applyGroupAggregation (List.zip flds kvals) group)
        |> Array.ofSeq )


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
      [| "Games", String r.Games; "Year", Number (float r.Year);  "Sport", String r.Sport
         "Discipline", String r.Discipline; "Athlete", String r.Athlete; "Team", String(countries.[r.Team]);
         "Gender", String r.Gender; "Event", String r.Event; "Medal", String r.Medal
         "Gold", Number (float r.Gold); "Silver", Number (float r.Silver); 
         "Bronze", Number (float r.Bronze) |]) |> Array.ofSeq )

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
          "Hodnota", hodnota |> float |> Number
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

let serializeValue = function 
  | Value.String s -> JsonValue.String s 
  | Value.Bool b -> JsonValue.Boolean b
  | Value.Number n -> JsonValue.Float n
  | Value.Date d -> JsonValue.String (d.ToString "o")

let serialize isPreview isSeries data = 
  let data = if isPreview then Array.truncate 10 data else data
  data 
  |> Array.map (fun (fields:_[]) ->
    if isSeries then
      JsonValue.Array [| serializeValue (snd fields.[0]); serializeValue (snd fields.[1]) |]
    else 
      fields |> Array.map (fun (k, v) -> k, serializeValue v) |> JsonValue.Record)
  |> JsonValue.Array

let applyAction isPreview meta objs = function
  | GetSeries(k, v) ->
      objs |> Seq.map (fun obj ->
        let kn, kval = Array.find (fst >> (=) k) obj
        let vn, vval = Array.find (fst >> (=) v) obj
        [| kn, kval; vn, vval |]) |> Array.ofSeq |> serialize isPreview true
  | GetTheData -> 
      objs |> serialize isPreview false
  | Metadata -> 
      JsonValue.Record [| for k, v in meta -> k, JsonValue.String v |]
  | GetRange(fld) ->
      objs |> Array.map (pickField fld) |> Array.distinct |> Array.map serializeValue |> JsonValue.Array


let app = pathScan "/%s" <| fun source -> request <| fun r -> 
  let source, meta = 
    if source = "olympics" then Olympics.allData.Value, Olympics.metadata
    elif source = "smlouvy" then Smlouvy.allData.Value, Smlouvy.metadata
    else failwith "Unknown source"
  let preview, query = r.query |> List.map fst |> List.partition ((=) "preview")
  let isPreview = not (List.isEmpty preview)
  let query = query |> List.head |> Transform.fromUrl

  let res = query.Transformations |> List.fold transformData (Seq.ofArray source) |> Array.ofSeq
  let json = applyAction isPreview meta res query.Action
  Successful.OK (json.ToString())  