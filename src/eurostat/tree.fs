namespace Eurostat
open System
open System.IO
open FSharp.Data
open System.Collections.Generic



module Tree =

  type Dataset =
    { Title : string
      Code : string
    }

  type Folder =
    { DatabaseByThemes : string
      Data : string
      Folders : List<Folder>
      Datasets : List<Dataset>
    }

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
  let treeFile = System.IO.File.ReadAllLines(treeFilePath)
  let (headers,structure, contents) = readHeaders treeFile
  let readDatasets =
    // let file = System.IO.File.ReadAllLines(treeFilePath)
    let datasets = contents |> Seq.filter (fun aLine -> isDataset(aLine)) |> Seq.map(fun x -> x.Replace("\"",""))
    let datasetNames = datasets |> Seq.map (fun aLine -> getDatasetCode(aLine)) 
    datasetNames
  
  let firstNonWhiteCharIndex (datasetTheme:string) =
    let chars = datasetTheme.ToCharArray()
    let index = Array.findIndex(fun c -> Char.IsLetterOrDigit(c)) chars
    index  

  let tokenizeLineContent (aLine:string) =
    let tokens = aLine.Split[|'\t'|]
    let databaseByThemes = tokens.[0]
    let data = tokens.[1]
    let folder = tokens.[2]
    (databaseByThemes, data, folder)

  let rec findFolder (root:Folder, folderCode:string) =
    match root with
    | [] -> printfn "Not found"
    | head:Folder :: tail -> printfn "%s" head.DatabaseByThemes 
                             findFolder (tail, folderCode)

  let rec readLines (contentsList, root:Folder, nonWhiteCharIndexOfPrevRow:int)=
    match contentsList with 
    | [] -> printfn "End"
    | head :: tail -> let (databaseByThemes, data, folder) = tokenizeLineContent head
                      let nonWhiteCharIndex = firstNonWhiteCharIndex databaseByThemes
                      if folder.Contains("dataset") then
                        let newDataset = {Title = databaseByThemes; Code=data}
                        root.Datasets.Add(newDataset)
                        readLines(tail, root, nonWhiteCharIndex)
                      else 
                        let newFolder = { DatabaseByThemes=databaseByThemes; Data=data; 
                                          Folders=new List<Folder>(); 
                                          Datasets=new List<Dataset>()}
                        
                        root.Folders.Add(newFolder)
                        if (nonWhiteCharIndex > nonWhiteCharIndexOfPrevRow) then
                          readLines(tail, newFolder, nonWhiteCharIndex) 
                        else 
                          readLines(tail, root, nonWhiteCharIndex) 
                                           
                   

  let readTree =
    let rootLine = Seq.head contents
    let branchesLines = Seq.tail contents
    let (databaseByThemes, data, folder) = tokenizeLineContent rootLine
    let root = { DatabaseByThemes=databaseByThemes; Data=data; 
                 Folders=new List<Folder>(); 
                 Datasets=new List<Dataset>()}
    readLines ((contents |> Seq.toList),root,0)
    printfn "Dataset: %A" root
    

  
      