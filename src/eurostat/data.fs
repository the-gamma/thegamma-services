namespace Eurostat
open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Dictionary


module Datasets =
  let dictionaries = getEurostatDictionaries

  let getFormat (firstLine:string) =
    if firstLine.Contains ("time\\geo") then "time\\geo"
    else if firstLine.Contains ("unit\\geo") then "unit\\geo"
    else if firstLine.Contains ("geo\\time") then "other\\time"
    else if firstLine.Contains ("citizen\\time") then "other\\time"
    else if firstLine.Contains ("time\\unit") then "time\\unit"
    else if firstLine.Contains ("unit\\time") then "other\\time"
    else if firstLine.Contains ("nace_r1\\time") then "other\\time"
    else if firstLine.Contains ("prof_pos\\time") then "other\\time"
    else "Nobody knows"

  let readOtherTimeHeaders (firstLine:string) = 
    let headersEnd = firstLine.IndexOf("\\time")
    let headers = firstLine.[0..headersEnd-1].Split [|','|] 
    let years = firstLine.[headersEnd+6..].Split [|'\t'|] |> Array.map(fun year -> year.Replace(" ","")) 
    (headers, years)

  let readOtherTimeRow (headers: string [], years: string [], aRow: string) = 
    let keysEnd = aRow.IndexOf("\t")
    let keys = aRow.[0..keysEnd-1].Split[|','|]
    let parsedKeys = keys |> Seq.mapi (fun i key -> getValue dictionaries headers.[i] key)
    let parsedKeysString = String.concat "," parsedKeys
    printf "." 
    let values = aRow.[keysEnd+1..].Split[|'\t'|] |> Array.map (fun value -> if value.Contains(":") then "0" else value.Replace(" ","")) 
    let yearValues = Array.map2(fun year value -> sprintf "%s,%s,%s" parsedKeysString year value) years values 
    yearValues
     
  let readOtherTimeRows (headers: string [], years: string [], contents: string []) = 
    let parsedRows = Array.collect (fun row  -> readOtherTimeRow(headers, years, row)) contents
    parsedRows

  let readTimeOtherHeaders (firstLine:string, other:string) = 
    let headersEnd = firstLine.IndexOf(other)
    let headers = firstLine.[0..headersEnd-1].Split [|','|] 
    let lengthOther = String.length other
    let otherUnit = firstLine.[headersEnd+lengthOther+1..].Split [|'\t'|] |> Array.map(fun unit -> unit.Replace(" ","")) 
    (headers, otherUnit)

  let readTimeOtherRow (headers: string [], other: string [], aRow: string, dictionary) = 
    let keysEnd = aRow.IndexOf("\t")
    let keys = aRow.[0..keysEnd-1].Split[|','|]
    let parsedOther = other |> Array.map (fun location -> getValue dictionaries dictionary location)
    let parsedKeys = keys |> Seq.mapi (fun i key -> getValue dictionaries headers.[i] key)
    printf "." 
    let parsedKeysString = String.concat "," parsedKeys
    let values = aRow.[keysEnd+1..].Split[|'\t'|] |> Array.map (fun value -> if value.Contains(":") then "0" else value.Replace(" ","")) 
    let yearValues = Array.map2(fun location value -> sprintf "%s,%s,%s" parsedKeysString location value) parsedOther values 
    yearValues
     
  let readTimeOtherRows (headers: string [], other: string [], contents: string [], dictionary) = 
    let parsedRows = Array.collect (fun row  -> readTimeOtherRow(headers, other, row, dictionary)) contents
    parsedRows

  let readFile fileName = 
    let contents = System.IO.File.ReadAllLines(fileName)
    let firstLine = Seq.head contents
    printfn "Summary: %A" firstLine 
    match getFormat(Seq.head contents) with
    | "other\\time" -> let (headers,years) = readOtherTimeHeaders firstLine
                       printfn "Headers: %A" headers 
                       printfn "Years: %A" years 
                       let parsedHeaders = String.concat "," headers 
                       let parsedFirstRow = sprintf "%s,year,value" parsedHeaders
                       let parsedRows = readOtherTimeRows (headers, years, (Seq.tail contents)|> Seq.toArray)
                       let dataset = Array.append [|parsedFirstRow|] parsedRows
                       dataset
    | "time\\geo" -> let (headers,geo) = readTimeOtherHeaders (firstLine,"\\geo")
                     let parsedHeaders = String.concat "," headers 
                     let parsedFirstRow = sprintf "%s,geo,value" parsedHeaders
                     let parsedRows = readTimeOtherRows (headers, geo, (Seq.tail contents)|> Seq.toArray, "geo")
                     let dataset = Array.append [|parsedFirstRow|] parsedRows
                     dataset  
    | "unit\\geo" -> let (headers,geo) = readTimeOtherHeaders (firstLine,"\\geo")
                     let parsedHeaders = String.concat "," headers 
                     let parsedFirstRow = sprintf "%s,unit,value" parsedHeaders
                     let parsedRows = readTimeOtherRows (headers, geo, (Seq.tail contents)|> Seq.toArray, "geo")
                     let dataset = Array.append [|parsedFirstRow|] parsedRows
                     dataset  
    | "time\\unit" -> let (headers,unit) = readTimeOtherHeaders (firstLine,"\\unit")
                      let parsedHeaders = String.concat "," headers 
                      let parsedFirstRow = sprintf "%s,unit,value" parsedHeaders
                      let parsedRows = readTimeOtherRows (headers, unit, (Seq.tail contents)|> Seq.toArray, "unit")
                      let dataset = Array.append [|parsedFirstRow|] parsedRows
                      dataset  
    | _ -> Array.empty<string>
  
  let writeFile dataset outputFile= 
    File.WriteAllLines(outputFile, dataset) 

  let readAndWriteFile fileName =
    let parsedFileName = sprintf "%s_parsed.csv" (System.IO.Path.GetFileNameWithoutExtension(fileName))
    let fileRoot = "/Users/myong/Documents/workspace/thegamma-services/data/eurostat/data/parsed"
    printfn "Start reading:%s" fileName
    let dataset = readFile fileName
    printfn "Sample Content:%s" dataset.[0]
    if not (Array.isEmpty dataset) then
      writeFile dataset (Path.Combine(fileRoot, parsedFileName))
      printfn "Parsing Complete: %s" parsedFileName 
      sprintf "Parsed Dataset: %s" parsedFileName 
    else 
      printfn "Parsing Failed: %s" parsedFileName 
      sprintf "\nFailed Dataset: %s\n" parsedFileName
    //dataset
    
  let readDirectory directoryPath =
    let filesArray = System.IO.Directory.GetFiles(directoryPath, "*.csv")      
    let subsetFilesArray = filesArray.[0..1]
    // printfn "%A" subsetFilesArray
    let logs = filesArray |> Array.map(fun file -> readAndWriteFile(file))
    let fileRoot = "/Users/myong/Documents/workspace/thegamma-services/data/eurostat/data"
    let logPath = (Path.Combine(fileRoot, "log.txt"))
    File.WriteAllLines(logPath, logs) 

    