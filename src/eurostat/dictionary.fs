namespace Eurostat
open System
open System.IO
open FSharp.Data
open System.Collections.Generic

module Dictionary =
  let filePath = "/Users/myong/Documents/workspace/thegamma-services/data/eurostat/dictionary/en"

  let getEurostatDictionaries =
    System.IO.Directory.GetFiles("/Users/myong/Documents/workspace/thegamma-services/data/eurostat/dictionary/en", "*.dic")
    |> Array.map (fun filepath -> System.IO.Path.GetFileName(filepath))

  let getDictionary dictionaries dictionary =
    let dictionaryFile = dictionaries |> Array.filter (fun (fileName:string) -> fileName.Contains(dictionary))
    dictionaryFile

  let getValue dictionaries dictionary key =
    let dictionary = getDictionary dictionaries dictionary
    match Array.length dictionary with
    | 0 -> "Term Not Found"
    | _ -> let contents = System.IO.File.ReadLines(Path.Combine(filePath, dictionary.[0])) |> Seq.toList
           let keyValues = contents |> List.filter (fun keyValue -> let aKey = keyValue.Split [|'\t'|]
                                                                    aKey.[0] = key )
           let keyValue = keyValues.[0].Split[|'\t'|]          
           keyValue.[1]