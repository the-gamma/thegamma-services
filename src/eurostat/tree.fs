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
    let databaseByThemesWithWhiteSpace = tokens.[0].Replace("\"","")
    let infoStartsHere = firstNonWhiteCharIndex databaseByThemesWithWhiteSpace
    let databaseByThemes = databaseByThemesWithWhiteSpace.[infoStartsHere..]
    let data = tokens.[1].Replace("\"","")
    let folder = tokens.[2].Replace("\"","")
    (databaseByThemes, data, folder)

  // let rec findFolder2 (rootFolders:List<Folder>, folderCode:string) =
  //   match rootFolders with
  //     | [] -> None
  //     | head::tail -> //printfn "%s" head.DatabaseByThemes
  //                     if head.DatabaseByThemes.Equals(folderCode) then
  //                       head
  //                     else  
  //                       findFolder (head.Folders, folderCode)
  //                     findFolder (tail, folderCode) 
  
  let getTypeName (x: 'T option) =
    typeof<'T>.Name
    
  // let rec findFolder (rootFolders:List<Folder>, folderCode:string) : Folder option=
  //   let result = rootFolders |> Seq.tryFind (fun f -> f.Data.Contains(folderCode))
  //   if result <> None then
  //     result
  //   else
  //     let mutable accumulator = 0
  //     let mutable results = null
  //     while (results = "None" && accumulator < rootFolders.Count) do
  //       results <- rootFolders.[accumulator].Folders |> Seq.tryFind (fun f -> f.Data.Contains(folderCode)) 
  //       accumulator <- accumulator + 1
  //     results
  let rec findFolder (aFolder:Folder, folderCode:string)=
    if (aFolder.Data.Contains(folderCode)) then
      aFolder
    elif (aFolder.Folders.Count = 0) then
      { DatabaseByThemes="databaseByThemes"; Data="data"; 
                 Folders = new List<Folder>(); 
                 Datasets= new List<Dataset>()} 
    else  
      // let results = aFolder.Folders |> Seq.map(fun f1 -> f1.Folders |> Seq.tryFind (fun f2 -> f2.Data.Contains(folderCode))) 
      // results |> Seq.iter(fun f -> printfn "Test: %A" f)
      // { DatabaseByThemes="databaseByThemes"; Data="data"; 
      //            Folders = new List<Folder>(); 
      //            Datasets= new List<Dataset>()} 
      let results = aFolder.Folders |> Seq.map(fun f -> findFolder(f, folderCode)) 
      let filteredresults = results |> Seq.filter(fun f -> f.DatabaseByThemes <> "databaseByThemes")
      // printfn "%i" (Seq.length filteredresults)
      if (Seq.isEmpty filteredresults) then
        { DatabaseByThemes="databaseByThemes"; Data="data"; 
                 Folders = new List<Folder>(); 
                 Datasets= new List<Dataset>()} 
      else 
        Seq.head filteredresults  
      

      
                                                                  
             

  // let rec findFolder (rootFolders:List<Folder>, folderCode:string) =
  //   if rootFolders.Count = 0 then
  //     None
  //   else 
  //     let folderFound = rootFolders |> Seq.tryFind (fun f -> f.Data.Contains(folderCode))
  //     if folderFound = None then
  //       for f in rootFolders do
  //         findFolder(f.Folders, folderCode)
  //       // results
  //     else
  //       folderFound

  // let rec findFolder (rootFolders:List<Folder>) (folderCode:string) : Folder option =
  //   match rootFolders with
  //     | Some t -> t |> Seq.tryFind (fun f -> f.Data.Contains(folderCode))

       
  
  // let rec findFolder (rootFolders:List<Folder>, folderCode:string) =
  //   let result = rootFolders |> Seq.tryFind (fun f -> f.Data.Contains(folderCode))
  //   for child in 
  //   result
    
          
    
  let getCodePrefix (code:string) =
    let lastIndexOfUnderscore = code.LastIndexOf("_")
    match lastIndexOfUnderscore with
      | -1 -> sprintf "Not found"
      | _ -> code.[0..lastIndexOfUnderscore-1]
  
  

  let rec readLines (contentsList, root:Folder, rootFolders:List<Folder>, rootDatasets:List<Dataset>) =
    match contentsList with 
      | [] -> printfn "End Reading Contents"
      | head :: tail -> let (databaseByThemes, data, folder) = tokenizeLineContent head
                        match folder with
                          | "dataset" -> let newDataset = {Title = databaseByThemes; Code=data}
                                         rootDatasets.Add(newDataset)
                                         //printfn "Dataset: %A" rootDatasets
                                         //readLines(tail, root, root.Folders, root.Datasets)
                          | _ -> let newFolder = { DatabaseByThemes=databaseByThemes; Data=data; 
                                          Folders=new List<Folder>(); 
                                          Datasets=new List<Dataset>()}
                                 let prefix = getCodePrefix data
                                 match prefix with
                                   | "Not found" -> root.Folders.Add(newFolder)  
                                   | _ -> let parentFolder = findFolder(root, prefix)
                                          // rootFolders.Add(newFolder)  
                                          match parentFolder.DatabaseByThemes with
                                            | "databaseByThemes" -> rootFolders.Add(newFolder)  
                                            | _ -> parentFolder.Folders.Add(newFolder)  
                        readLines(tail, root, root.Folders, root.Datasets)
                                           
  let readTree =
    let rootLine = Seq.head contents
    let branchesLines = Seq.tail contents
    let (databaseByThemes, data, folder) = tokenizeLineContent rootLine
    let root = { DatabaseByThemes=databaseByThemes; Data=data; 
                 Folders = new List<Folder>(); 
                 Datasets= new List<Dataset>()} 
    readLines ((contents |> Seq.toList), root, root.Folders, root.Datasets)
    printfn "Datasets: %A" root
    
    

  
      