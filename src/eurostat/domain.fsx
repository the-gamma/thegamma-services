
//namespace Eurostat
#if INTERACTIVE
#I "../../packages"
#r "System.Xml.Linq.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "Suave/lib/net40/Suave.dll"
#load "request.fs"
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

module Domain =
  let fileName = "rd_p_perslf.csv"
  let parsedFileName = sprintf "%s_parsed.csv" (System.IO.Path.GetFileNameWithoutExtension(fileName))
  let fileRoot = "/Users/myong/Documents/workspace/thegamma-services/data/eurostat/data"
  let dataset = readFile (Path.Combine(fileRoot, fileName))
  
  writeFile dataset (Path.Combine(fileRoot, parsedFileName))
  // let datasetName = "rd_p_perslf"
  // getDataset datasetName
 