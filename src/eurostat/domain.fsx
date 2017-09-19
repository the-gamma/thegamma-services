
//namespace Eurostat
#if INTERACTIVE
#I "../../packages"
#r "System.Xml.Linq.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "Suave/lib/net40/Suave.dll"
#load "request.fs"
#load "tree.fs"
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
open Eurostat.Requests
open Eurostat.Tree

module Domain =
  

  // Gets the list of dataset names
  // let datasets = readTree
  // Seq.iter(fun d -> printfn "%s" d) datasets
  
  // Downloads datasets of that name
  // let downloads = Seq.iter(fun code -> getDataset(code)) datasets
  // downloads

  // Unzip
  // Does not work

  // Parses datasets
  // let fileName = "rd_p_perslf.csv"
  // let parsedFileName = sprintf "%s_parsed.csv" (System.IO.Path.GetFileNameWithoutExtension(fileName))
  let fileRoot = "/Users/myong/Documents/workspace/thegamma-services/data/eurostat/data/unzipped"
  // let dataset = readFile fileRoot
  // writeFile dataset (Path.Combine(fileRoot, parsedFileName))
  (readDirectory fileRoot)