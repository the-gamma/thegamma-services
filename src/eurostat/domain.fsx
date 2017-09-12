
//namespace Eurostat
#if INTERACTIVE
#I "../../packages"
#r "System.Xml.Linq.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "Suave/lib/net40/Suave.dll"
#load "dictionary.fs"
#load "data.fs"
#else
module Services.Eurostat
#endif
#nowarn "1104"

open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Eurostat.Datasets

module Domain =
  let dataset = readFile
  printfn "%A" dataset
  writeFile dataset
 