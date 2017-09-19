namespace Eurostat
open System
open System.IO
open FSharp.Data
open System.Collections.Generic

module Tree =

  let readHeaders (contents:seq<string>)= 
    let firstLine:string = Seq.head contents
    let headers = firstLine.Split [|'\t'|] |> Seq.map(fun x -> x.Replace("\"",""))
    let otherLines = Seq.tail contents
    let secondLine:string = Seq.head otherLines
    let structure = secondLine.Split [|'\t'|] |> Seq.map(fun x -> x.Replace("\"",""))
    let contents = Seq.tail otherLines
    (headers, structure, contents)

  // let (=?) s1 s2 = System.String.Equals(s1, s2, System.StringComparison.CurrentCultureIgnoreCase)

  let isDataset (aLine:string) =
    let tokens = aLine.Split[|'\t'|]
    let datatype = tokens.[2]
    datatype.Contains("dataset")
  
  let getDatasetCode (aLine:string) =
    let tokens = aLine.Split[|'\t'|]
    let code = tokens.[1]
    code
     
  let treeFilePath = "/Users/myong/Documents/workspace/thegamma-services/data/eurostat/eurostat-science.txt"
  let readTree =
    let file = System.IO.File.ReadAllLines(treeFilePath)
    let (headers,structure, contents) = readHeaders file
    let datasets = contents |> Seq.filter (fun aLine -> isDataset(aLine)) |> Seq.map(fun x -> x.Replace("\"",""))
    let datasetNames = datasets |> Seq.map (fun aLine -> getDatasetCode(aLine)) 
    datasetNames

  
      