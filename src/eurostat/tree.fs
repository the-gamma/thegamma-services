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

  let rec findFolder (rootFolders:List<Folder>, folderCode:string) =
    match rootFolders with
    | [] -> None
    | head::tail -> //printfn "%s" head.DatabaseByThemes
                    if head.DatabaseByThemes.Equals(folderCode) then
                      head
                    else  
                      findFolder (head.Folders, folderCode)
                    findFolder (tail, folderCode) 
  
  let rec findFolder2 (rootFolders:List<Folder>, folderCode:string) : Folder =
    match rootFolders.Count with
    | 0 -> None
    | _ -> let head = Seq.head rootFolders
           let tail = (Seq.tail rootFolders) |> Seq.toList
           if head.DatabaseByThemes.Equals(folderCode) then
             head
           findFolder (tail, folderCode) 

  let getCodePrefix (code:string) =
    let tokens = code.Split[|'\t'|]
    match tokens.Length with
    | 0 -> sprintf "Not found"
    | _ -> tokens.[0] 

  let rec readLines (contentsList, root:Folder, rootFolders:List<Folder>, rootDatasets:List<Dataset>) =
    match contentsList with 
    | [] -> printfn "End"
    | head :: tail -> let (databaseByThemes, data, folder) = tokenizeLineContent head
                      match folder with
                      | "dataset" -> let newDataset = {Title = databaseByThemes; Code=data}
                                     rootDatasets.Add(newDataset)
                                     readLines(tail, root, root.Folders, root.Datasets)
                      | _ -> let newFolder = { DatabaseByThemes=databaseByThemes; Data=data; 
                                          Folders=new List<Folder>(); 
                                          Datasets=new List<Dataset>()}
                             let prefix = getCodePrefix data
                             match prefix with
                             | "Not found" -> rootFolders.Add(newFolder)  
                             | _ -> let parentFolder:Folder = findFolder(root.Folders, prefix)
                                    parentFolder.Folders.Add(newFolder)
                      readLines(tail, root, root.Folders, root.Datasets)
                                           
  let readTree =
    let rootLine = Seq.head contents
    let branchesLines = Seq.tail contents
    let (databaseByThemes, data, folder) = tokenizeLineContent rootLine
    let root = { DatabaseByThemes=databaseByThemes; Data=data; 
                 Folders = new List<Folder>(); 
                 Datasets= new List<Dataset>(); 
    Folders
    readLines ((contents |> Seq.toList), root, root.Folders, root.Datasets)
    printfn "Dataset: %A" root
    

  
      