#if INTERACTIVE
#I "../../packages"
#r "System.Xml.Linq.dll"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "Suave/lib/net40/Suave.dll"
#load "../serializer.fs"
#load "eurostat-sci-domain.fs"
#else
module Services.Eurostat
#endif
#nowarn "1104"
open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Eurostat
open Eurostat.Domain
open Services.Serializer

let eurostatScience = Domain.getTree

open Suave
open Suave.Filters
open Suave.Operators

type ThingSchema = { ``@context``:string; ``@type``:string; name:string; }
type GenericType = { name:string; ``params``:obj[] }
type TypePrimitive = { kind:string; ``type``:obj; endpoint:string }
type TypeNested = { kind:string; endpoint:string }
type Member = { name:string; returns:obj; trace:string[]; schema:ThingSchema }

let noSchema = Unchecked.defaultof<ThingSchema>
let makeSchemaThing kind name =
  { ``@context`` = "http://schema.org/"; ``@type`` = kind; name = name }
let makeSchemaExt kind name =
  { ``@context`` = "http://thegamma.net/worldbank"; ``@type`` = kind; name = name }

let memberPath s f = 
  path s >=> request (fun _ -> f() |> Array.ofSeq |> toJson |> Successful.OK)

let memberPathf fmt f = 
  pathScan fmt (fun b -> f b |> Array.ofSeq |> toJson |> Successful.OK)

let (|Lookup|_|) k (dict:IDictionary<_,_>) =
  match dict.TryGetValue k with
  | true, v -> Some v
  | _ -> None

let app =
  printfn "%A" eurostatScience