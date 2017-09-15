namespace Eurostat
open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Dictionary


module Datasets =
  let dictionaries = getEurostatDictionaries
  let readHeaders (firstLine:string) = 
    let headersEnd = firstLine.IndexOf("\\time")
    let headers = firstLine.[0..headersEnd-1].Split [|','|] 
    let years = firstLine.[headersEnd+6..].Split [|'\t'|] 
    (headers, years)

  let readRow (headers: string [], years: string [], aRow: string) = 
    let keysEnd = aRow.IndexOf("\t")
    let keys = aRow.[0..keysEnd-1].Split[|','|]
    let parsedKeys = keys |> Seq.mapi (fun i key -> getValue dictionaries headers.[i] key)
    let parsedKeysString = String.concat "," parsedKeys
    let values = aRow.[keysEnd+1..].Split[|'\t'|] |> Array.map (fun value -> if value.Contains(":") then "0" else value.Replace(" ","")) 
    let yearValues = Array.map2(fun year value -> sprintf "%s,%s,%s" parsedKeysString year value) years values 
    yearValues
     
  let readRows (headers: string [], years: string [], contents: string []) = 
    let parsedRows = Array.collect (fun row  -> readRow(headers, years, row)) contents
    parsedRows

  let readFile fileName = 
    let contents = System.IO.File.ReadAllLines(fileName)

    let (headers,years) = readHeaders (Seq.head contents)
    let parsedHeaders = String.concat "," headers 
    let parsedFirstRow = sprintf "%s,year,value" parsedHeaders

    let parsedRows = readRows (headers, years, (Seq.tail contents)|> Seq.toArray)
    let dataset = Array.append [|parsedFirstRow|] parsedRows
    dataset
  
  let writeFile dataset outputFile= 
    File.WriteAllLines(outputFile, dataset) 
      