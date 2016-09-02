module Services.Facets
open Services.Serializer
#nowarn "1104"

// ----------------------------------------------------------------------------
// Working with Schema.org annotations
// ----------------------------------------------------------------------------
 
type ThingSchema = { ``@context``:string; ``@type``:string; name:string; }
type CollectionSchema = { ``@type``:string; name:string }
type AddActionSchema = { ``@context``:string; ``@type``:string; targetCollection:CollectionSchema }
type CreateActionSchema = { ``@context``:string; ``@type``:string; result:CollectionSchema }
  
let makeThingSchema kind name =
  { ``@context`` = "http://schema.org/"; ``@type`` = kind; name = name }
let makeCreateSchema name =
  let col = { CollectionSchema.``@type`` = "ItemList"; name=name }
  { ``@context`` = "http://schema.org/"; ``@type`` = "CreateAction"; result = col }
let makeAddSchema name =
  let col = { CollectionSchema.``@type`` = "ItemList"; name=name }
  { ``@context`` = "http://schema.org/"; ``@type`` = "AddAction"; targetCollection = col }
let noSchema = Unchecked.defaultof<ThingSchema>


// ----------------------------------------------------------------------------
// REST protocol domain model
// ----------------------------------------------------------------------------

type RecordField = { name:string; ``type``:obj }
type GenericType = { name:string; ``params``:obj[] }
type RecordType = { name:string (* = record *); fields:RecordField[] }
type TypePrimitive = { kind:string; ``type``:obj; endpoint:string }
type TypeNested = { kind:string; endpoint:string }
type Documentation = { title:string; details:string }
type Member = { name:string; returns:obj; trace:string[]; schema:obj; documentation:obj (*null+Documentation*) }


// ----------------------------------------------------------------------------
// Facets
// ----------------------------------------------------------------------------

open System.Collections.Generic

type Facet<'T> = 
  | Filter of string * multichoice:bool * ('T -> option<string * ThingSchema>)
  | Choice of string * seq<string * string * ThingSchema * Facet<'T>>

let rec findFilter path facet = 
  match path, facet with
  | [], f -> f
  | p::ps, Choice(_, choices) -> 
      match choices |> Seq.tryPick (fun (k, _, _, f) -> if p = k  then Some(findFilter ps f) else None) with
      | Some f -> f
      | None -> failwithf "Could not find filter '%s' in choices '%A'" p [ for c, _, _, _ in choices -> c ]
  | _ -> failwith "Mismatching filter"

let findFacet (facetsLookup:IDictionary<_, _>) prefix = 
  match prefix with
  | [] -> failwith "Empty facet"
  | f::fs -> findFilter fs facetsLookup.[f]


// ----------------------------------------------------------------------------
// Suave server
// ----------------------------------------------------------------------------

open Suave
open Suave.Filters
open Suave.Operators
open System
open System.Text
open FSharp.Data

let (|Let|) a v = a, v

let (|SplitBy|_|) k l = 
  let rec loop acc = function
    | x::xs when x = k -> Some(List.rev acc, xs)
    | x::xs -> loop (x::acc) xs
    | [] -> None
  loop [] l

let (</>) (a:string) (b:string) = 
  let a, b = a.Trim('/'), b.Trim('/')
  if a = "" then "/" + b 
  elif b = "" then "/" + a
  else "/" + a + "/" + b

let createFacetApp records fields facets = 

  let mutable cachedOptions = Map.empty
  let getOptions name f = 
      printfn "Get options %s" name 
      match cachedOptions.TryFind name with
      | Some res -> res
      | None -> 
          let res = records |> Array.choose f |> Array.distinctBy fst
          printfn "%A" res
          cachedOptions <- Map.add name res cachedOptions
          res

  let facetLookup = dict facets

  request (fun r ->
    let selected = r.url.LocalPath.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq
    match selected with
    | ["data"] ->
        let data = (Utils.ASCII.toString r.rawForm)
        let constraints = 
          data.Split([|'&'|], StringSplitOptions.RemoveEmptyEntries) 
          |> Array.map (fun kvp -> 
              let kv = kvp.Split('=') in 
              kv.[0].Split('/')
              |> List.ofSeq
              |> List.map System.Web.HttpUtility.UrlDecode, System.Web.HttpUtility.UrlDecode(kv.[1]))
          |> Array.groupBy fst
          |> Array.map (fun (facet, values) ->
              findFacet facetLookup facet, set (Seq.map snd values))

        records
        |> Array.filter (fun r ->
            constraints |> Array.forall (fun (f, vs) -> 
              match f with 
              | Filter(_, _, f) -> match f r with Some(v, _) -> vs.Contains v | _ -> false
              | _ -> false))
        |> Array.map (fun r -> 
            fields 
            |> Array.map (fun (name, typ, getter) -> name, JsonValue.String(string (getter r)))
            |> JsonValue.Record )
        |> JsonValue.Array
        |> string        
        |> Successful.OK

    | Let true (nested, SplitBy "and-pick" (rest, path))
    | Let false (nested, SplitBy "pick" (rest, path)) ->
        let prefix = if nested then "or " else ""
        match findFacet facetLookup path with
        | Filter(name, multichoice, f) ->
            let options = getOptions name f
            let andPickTy = 
              { kind="nested"; endpoint=(List.fold (</>) "" rest) </> "and-pick" </> (List.fold (</>) "" path) }
            let thenTy = 
              { kind="nested"; endpoint=(List.fold (</>) "" rest) </> List.head path }
            let doc = { title="Select " + name; details="In the list below, you can choose an individual " + name + ":" }

            [ for (value, schema) in options do
                let schema = 
                  if multichoice then makeAddSchema "List" |> box
                  else schema |> box
                let tpath = path |> List.map System.Web.HttpUtility.UrlEncode
                let ty = if multichoice then andPickTy else thenTy
                let trace = [| String.concat "/" tpath + "=" + System.Web.HttpUtility.UrlEncode value |]
                yield { name=prefix + value; schema=schema; returns=ty; trace=trace; documentation=doc }
              if multichoice then 
                yield { name="then"; returns=thenTy; schema=noSchema; trace=[| |]; documentation=null } ]
            |> Array.ofSeq |> toJson |> Successful.OK 

        | Choice(name, c) ->
            let doc = { title="Select " + name; details="First, select a " + name + ". There are too many items, so " + name + " is used as a category to make finding individual items in the list easier. " }
            c
            |> Seq.map (fun (k, v, s, facet) ->
                let schema = 
                  match facet with
                  | Filter(_, true, _) -> makeCreateSchema "List" |> box
                  | _ -> s |> box
                { name=v; returns= {kind="nested"; endpoint= (List.fold (</>) "" selected) </> k }
                  schema=schema; trace=[| |]; documentation=doc })
            |> Array.ofSeq |> toJson |> Successful.OK             

    | selected ->
        facets 
        |> Seq.filter (fun (facetKey, _) -> 
            selected |> Seq.forall (fun sel -> not (sel.StartsWith(facetKey))))
        |> Seq.map (fun (facetKey, facet) ->
            let doc, schema = 
              match facet with
              | Filter(n, true, _) -> 
                  box { title="Select " + n; details="In the following list, you can select one or more " + n + ". Use the drop-down to add more " + n + ". When removing " + n + ", you can only remove the first item once you remove all later items." },
                  makeCreateSchema "List" |> box
              | Filter(n, _, _)
              | Choice(n, _) -> null, noSchema |> box
            { name="by " + facetKey; returns= {kind="nested"; endpoint=r.url.LocalPath </> "pick" </> facetKey}
              schema=schema; trace=[| |]; documentation=doc })
        |> Seq.append [
            ( let flds = [| for name, typ, getter in fields -> { name=name; ``type``=typ }  |]
              let typ = { name="seq"; ``params``=[| { name="record"; fields=flds } |] }
              { name="data"; returns= {kind="primitive"; ``type``=typ; endpoint="/data"}
                schema=noSchema; trace=[| |]; documentation=null } ) ]
        |> Array.ofSeq |> toJson |> Successful.OK )
