#if INTERACTIVE
#r "System.Xml.Linq.dll"
#r "System.IO.Compression.FileSystem.dll"
#I "../../packages"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Suave/lib/net40/Suave.dll"
#r "FSharp.Control.AsyncSeq/lib/net45/FSharp.Control.AsyncSeq.dll"
#else
module Services.PivotData
#endif
#nowarn "1104"
open System
open System.IO
open FSharp.Data
open FSharp.Control
open System.Text.RegularExpressions

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
  | Empty

type Action = 
  | Metadata
  | GetSeries of string * string
  | GetTheData 
  | GetRange of string

type Query = 
  { Transformations : Transformation list
    Action : Action }

module Transform = 

  let ops = 
    [ "count-dist", CountDistinct; "unique", ReturnUnique; 
      "concat-vals", ConcatValues; "sum", Sum; "mean", Mean ]

  let trimIdent (s:string) = 
    if s.StartsWith("'") && s.EndsWith("'") then s.Substring(1, s.Length-2)
    else s

  let parseAggOp op =
    if op = "key" then GroupKey
    elif op = "count-all" then CountAll
    else
      let parsed = ops |> List.tryPick (fun (k, f) ->
        if op.StartsWith(k) then Some(f(trimIdent(op.Substring(k.Length + 1))))
        else None)
      if parsed.IsSome then parsed.Value else failwith "Unknonw operation"

  let parseAction (op, args) = 
    match op, args with
    | "metadata", [] -> Metadata, true
    | "series", [k; v] -> GetSeries(k, v), true
    | "range", [f] -> GetRange(f), true
    | _ -> GetTheData, false

  let parseCondition (cond:string) = 
    let cond = cond.Trim()
    let start = if cond.StartsWith("'") then cond.IndexOf('\'', 1) else 0
    let neq, eq = cond.IndexOf(" neq ", start), cond.IndexOf(" eq ", start)
    if neq <> -1 then trimIdent (cond.Substring(0, neq)), false, cond.Substring(neq + 5)
    elif eq <> -1 then trimIdent (cond.Substring(0, eq)), true, cond.Substring(eq + 4)
    else failwith "Incorrectly formatted condition"

  let parseTransform (op, args) = 
    match op, args with
    | "drop", columns -> DropColumns(List.map trimIdent columns)
    | "sort", columns -> SortBy(columns |> List.map (fun col -> 
        if col.EndsWith(" asc") then trimIdent (col.Substring(0, col.Length-4)), Ascending
        elif col.EndsWith(" desc") then trimIdent (col.Substring(0, col.Length-5)), Descending
        else trimIdent col, Ascending))
    | "filter", conds -> FilterBy(List.map parseCondition conds)
    | "groupby", ops ->
        let keys = ops |> List.takeWhile (fun s -> s.StartsWith "by ") |> List.map (fun s -> trimIdent (s.Substring(3)))
        let aggs = ops |> List.skipWhile (fun s -> s.StartsWith "by ") |> List.map parseAggOp
        GroupBy(keys, aggs)
    | "take", [n] -> Paging [Take (int n)]
    | "skip", [n] -> Paging [Skip (int n)]
    | _ -> failwith (sprintf "Unsupported transformation %s %A" op args)

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
      let check a e b = if e then a = b else a <> b
      conds |> List.fold (fun objs (fld, eq, value) ->
        objs |> Seq.filter (fun o -> 
          match pickField fld o with
          | String v -> check v eq value
          | Number n -> check n eq (decimal value))) objs
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
// Loading data for stackoverflow survey 
// ----------------------------------------------------------------------------

module StackOverflow =
  let (</>) a b = Path.Combine(a, b) 
  let cache = 
    if System.Reflection.Assembly.GetExecutingAssembly().IsDynamic then 
      __SOURCE_DIRECTORY__ + "/../../cache"
    else IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location) + "/../cache"
  if not (Directory.Exists cache) then Directory.CreateDirectory cache |> ignore

  let files = 
    [
//      ("2012", "2012 Stack Overflow Survery Results.csv", @"https://drive.google.com/uc?export=download&id=0B0DL28AqnGsrX3JaZWVwWEpHNWM")
//      ("2013", "2013 Stack Overflow Survey Responses.csv", @"https://drive.google.com/uc?export=download&id=0B0DL28AqnGsrenpPNTc5UE1PYW8")
//      ("2014", "2014 Stack Overflow Survey Responses.csv", @"https://drive.google.com/uc?export=download&id=0B0DL28AqnGsrempjMktvWFNaQzA")
//      ("2015", "2015 Stack Overflow Developer Survey Responses.csv", @"https://drive.google.com/uc?export=download&id=0B0DL28AqnGsra1psanV1MEdxZk0")
      ("2016", "2016 Stack Overflow Survey Results/2016 Stack Overflow Survey Responses.csv", @"https://drive.google.com/uc?export=download&id=0B0DL28AqnGsrV0VldnVIT1hyb0E")
    ]

  let updateCache() = async {
    use wc = new System.Net.WebClient()
    for (n, f, u) in files do
      let target = cache </> (n + ".zip")
      if not (File.Exists(target)) then 
        do! wc.AsyncDownloadFile(System.Uri(u), target) 
        Compression.ZipFile.ExtractToDirectory(target, cache </> n) }

  let (|Match|_|) (pat:string) (inp:string) =
    let m = Regex.Match(inp, pat) in
    if m.Success
    then Some (List.tail [ for g in m.Groups -> g.Value ])
    else None
  let (|Match1|_|) (pat:string) (inp:string) =
    match (|Match|_|) pat inp with
    | Some (fst :: []) -> Some fst
    | Some [] -> failwith "Match1 succeeded, but no groups found. Use '(.*)' to capture groups"
    | Some _ -> failwith "Match1 succeeded, but did not find exactly one match."
    | None -> None  
  let (|Match2|_|) (pat:string) (inp:string) =
    match (|Match|_|) pat inp with
    | Some (fst :: snd :: []) -> Some (fst, snd)
    | Some [] -> failwith "Match2 succeeded, but no groups found. Use '(.*)' to capture groups"
    | Some _ -> failwith "Match2 succeeded, but did not find exactly two matches."
    | None -> None  
  let (|Match3|_|) (pat:string) (inp:string) =
    match (|Match|_|) pat inp with
    | Some (fst :: snd :: trd :: []) -> Some (fst, snd, trd)
    | Some [] -> failwith "Match3 succeeded, but no groups found. Use '(.*)' to capture groups"
    | Some _ -> failwith "Match3 succeeded, but did not find exactly three matches."
    | None -> None  

  let stripCurrency (value: string) = value.Replace("$", "")
  let stripComma (value: string) = value.Replace(",", "")
  let money s = s |> stripCurrency |> stripComma |> decimal |> Number 
  let moneyRange (value: string) = 
    match value.Split([|' '|]) |> List.ofArray with
    | ["Less"; "than"; top] -> (None, Some (money top))
    | ["More"; "than"; buttom] -> (Some (money buttom), None)
    | [buttom; "-"; top] -> (Some (money buttom), Some (money top))
    | _ -> (None, None)
  let decN = decimal >> Number
  let intRange (value: string) = 
    match value.Replace(" ", "") with
    | Match1 "<(.*)" top -> (None, Some (decN top))
    | Match1 ">(.*)" buttom -> (Some (decN buttom), None)
    | Match2 "(.*)-(.*)" (buttom, top) -> (Some (decN buttom), Some (decN top))
    | _ -> (None, None)
  let rfrom (b, t) = match (b) with | Some(n) -> n | None -> Number 0m
  let rto (b, t) = match (t) with | Some(n) -> n | None -> Number 0m
  let someDecimal d = 
    match d with
    | Some n -> Number n
    | None -> Number 0m
  let strNA s = String (if String.IsNullOrEmpty(s) then "N/A" else s)

  let [<Literal>] Sample = __SOURCE_DIRECTORY__ + "/../../data/stackoverfloe-survey-sample.csv"
  type Survey = CsvProvider<Sample, CacheRows=false, Schema=",collector,country,un_subregion,so_region,age_range,age_midpoint=decimal option,gender,self_identification,occupation,occupation_group,experience_range,experience_midpoint=decimal option,salary_range,salary_midpoint=decimal option,big_mac_index=decimal option,tech_do,tech_want,aliens,programming_ability=decimal option,employment_status,industry,company_size_range,team_size_range,women_on_team,remote,job_satisfaction,job_discovery,dev_environment,commit_frequency,hobby,dogs_vs_cats,desktop_os,unit_testing,rep_range,visit_frequency,why_learn_new_tech,education,open_to_new_job,new_job_value,job_search_annoyance,interview_likelihood,how_to_improve_interview_process,star_wars_vs_star_trek,agree_tech,agree_notice,agree_problemsolving,agree_diversity,agree_adblocker,agree_alcohol,agree_loveboss,agree_nightcode,agree_legacy,agree_mars,important_variety,important_control,important_sameend,important_newtech,important_buildnew,important_buildexisting,important_promotion,important_companymission,important_wfh,important_ownoffice,developer_challenges,why_stack_overflow">

  let readFiles() = 
    asyncSeq {
      for (n, f, _) in files do 
        let target = cache </> n </> f
        printfn "reading %s" target
        let! results = Survey.AsyncLoad(cache </> n </> f)
        yield 
          n,
          results.Rows |> Seq.map (fun r ->
          let salaryRange = moneyRange r.Salary_range
          let ageRange = intRange r.Age_range
          [| "Collector", String r.Collector
             "Country", String r.Country 
             "Subregion", String r.Un_subregion
             "SO Region", String r.So_region
             "Age Midpoint", someDecimal r.Age_midpoint
             "Age From", rfrom ageRange
             "Age To", rto ageRange
             "Gender", strNA r.Gender
             "Self Identitification", String r.Self_identification
             "Occupation", String r.Occupation
             "Occupation Group", String r.Occupation_group
             "Experience Range", String r.Experience_range
             "Experience Midpoint", someDecimal r.Experience_midpoint
             "Salary Range", String r.Salary_range
             "Salary Midpoint", someDecimal r.Salary_midpoint
             "Salary From", rfrom salaryRange
             "Salary To", rto salaryRange
             "Big Max Index", someDecimal r.Big_mac_index
             "Tech Do", String r.Tech_do
             "Tech Want", String r.Tech_want
             "Aliens", String r.Aliens
             "Programming Ability", someDecimal r.Programming_ability
             "Employment Status", String r.Employment_status
             "Industry", String r.Industry
             "Sompany Size Range", String r.Company_size_range
             "Team Size Range", String r.Team_size_range
             "Women On Team", String r.Women_on_team
             "Remote", String r.Remote
             "Job Satisfaction", String r.Job_satisfaction
             "Job Discovery", String r.Job_discovery
             "Development Environment", String r.Dev_environment
             "Commit Frequency", String r.Commit_frequency
             "Hobby", String r.Hobby
             "Dogs vs Cats", strNA r.Dogs_vs_cats
             "Desktop OS", strNA r.Desktop_os
             "Unit Testing", String r.Unit_testing
             "SO Reputation Range", String r.Rep_range
             "SO Visit Frequency", String r.Visit_frequency
             "Why Learn New Tech", String r.Why_learn_new_tech
             "Education", String r.Education
             "Open To New Job", String r.Open_to_new_job
             "New Job Value", String r.New_job_value
             "Job Search Annoyance", String r.Job_search_annoyance
             "Interview Likelihood", String r.Interview_likelihood
             "How To Improve Interview Process", String r.How_to_improve_interview_process
             "Star Wars vs Star Trek", String r.Star_wars_vs_star_trek
             "Agree Tech", String r.Agree_tech
             "Agree Notice", String r.Agree_notice
             "Agree Problem Solving", String r.Agree_problemsolving
             "Agree Diversity", String r.Agree_diversity
             "Agree AdBlocker", String r.Agree_adblocker
             "Agree Alcohol", String r.Agree_alcohol
             "Agree Love Boss", String r.Agree_loveboss
             "Agree Night Code", String r.Agree_nightcode
             "Agree Legacy", String r.Agree_legacy
             "Agree Mars", String r.Agree_mars
             "Important Variety", String r.Important_variety
             "Important Control", String r.Important_control
             "Important Same End", String r.Important_sameend
             "Important New Tech", String r.Important_newtech
             "Important Build New", String r.Important_buildnew
             "Important Build Existing", String r.Important_buildexisting
             "Important Promotion", String r.Important_promotion
             "Important Company Mission", String r.Important_companymission
             "Important Work From Home", String r.Important_wfh
             "Important Own Office", String r.Important_ownoffice
             "Developer Challenges", String r.Developer_challenges
             "Why Stack Overflow", String r.Why_stack_overflow
          |]) |> Array.ofSeq
    } |> AsyncSeq.toArray |> dict

  let allData = Lazy.Create(fun () ->
    updateCache() |> Async.RunSynchronously
    readFiles() )

  let metadata =
    [ "Collector", "string"
      "Country", "string"
      "Subregion", "string"
      "SO Region", "string"
      "Age Midpoint", "number"
      "Age From", "number"
      "Age To", "number"
      "Occupation", "string"
      "Gender", "string"
      "Self Identitification", "string"
      "Occupation Group", "string"
      "Experience Range", "string"
      "Experience Midpoint", "string"
      "Salary Range", "string"
      "Salary Midpoint", "number"
      "Salary From", "number"
      "Salary To", "number"
      "Big Max Index", "number"
      "Tech Do", "string"
      "Tech Want", "string"
      "Aliens", "string"
      "Programming Ability", "number"
      "Employment Status", "string"
      "Industry", "string"
      "Sompany Size Range", "string"
      "Team Size Range", "string"
      "Women On Team", "string"
      "Remote", "string"
      "Job Satisfaction", "string"
      "Job Discovery", "string"
      "Development Environment", "string"
      "Commit Frequency", "string"
      "Hobby", "string"
      "Dogs vs Cats", "string"
      "Desktop OS", "string"
      "Unit Testing", "string"
      "SO Reputation Range", "string"
      "SO Visit Frequency", "string"
      "Why Learn New Tech", "string"
      "Education", "string"
      "Open To New Job", "string"
      "New Job Value", "string"
      "Job Search Annoyance", "string"
      "Interview Likelihood", "string"
      "How To Improve Interview Process", "string"
      "Star Wars vs Star Trek", "string"
      "Agree Tech", "string"
      "Agree Notice", "string"
      "Agree Problem Solving", "string"
      "Agree Diversity", "string"
      "Agree AdBlocker", "string"
      "Agree Alcohol", "string"
      "Agree Love Boss", "string"
      "Agree Night Code", "string"
      "Agree Legacy", "string"
      "Agree Mars", "string"
      "Important Variety", "string"
      "Important Control", "string"
      "Important Same End", "string"
      "Important New Tech", "string"
      "Important Build New", "string"
      "Important Build Existing", "string"
      "Important Promotion", "string"
      "Important Company Mission", "string"
      "Important Work From Home", "string"
      "Important Own Office", "string"
      "Developer Challenges", "string"
      "Why Stack Overflow", "string"
    ] 

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
  printfn "%s %A" source r.query
  let source, meta = 
    if source = "olympics" then Olympics.allData.Value, Olympics.metadata
    elif source = "smlouvy" then Smlouvy.allData.Value, Smlouvy.metadata
    elif source = "stackoverflow2016" then StackOverflow.allData.Value.["2016"], StackOverflow.metadata
    else failwith "Unknown source"
  let preview, query = r.query |> List.map fst |> List.partition ((=) "preview")
  let isPreview = not (List.isEmpty preview)
  let query = query |> List.head |> Transform.fromUrl

  let res = query.Transformations |> List.fold transformData (Seq.ofArray source) |> Array.ofSeq
  let json = applyAction isPreview meta res query.Action
  Successful.OK (json.ToString())  