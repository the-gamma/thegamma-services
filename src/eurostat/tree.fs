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
     
  let treeFilePath = "/Users/myong/Documents/workspace/thegamma-services/src/eurostat/data/eurostat-science-full.txt"
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
    (databaseByThemesWithWhiteSpace, data, folder)
  
  let getTypeName (x: 'T option) =
    typeof<'T>.Name          
  let rec findFolder (aFolder:Folder, folderCode:string)=
    if (aFolder.Data.StartsWith(folderCode)) then
      aFolder
    elif (aFolder.Folders.Count = 0) then
      { DatabaseByThemes="databaseByThemes"; Data="data"; 
                 Folders = new List<Folder>(); 
                 Datasets= new List<Dataset>()} 
    else   
      let results = aFolder.Folders |> Seq.map(fun f -> findFolder(f, folderCode)) 
      let filteredresults = results |> Seq.filter(fun f -> f.DatabaseByThemes <> "databaseByThemes")
      if (Seq.isEmpty filteredresults) then
        { DatabaseByThemes="databaseByThemes"; Data="data"; 
                 Folders = new List<Folder>(); 
                 Datasets= new List<Dataset>()} 
      else 
        // printfn "searching for %s" folderCode
        // filteredresults |> Seq.iter(fun f -> printfn "Found: %s" f.DatabaseByThemes)
        Seq.head filteredresults  
  

  let findIndexOfRow (row:string) =
    let contentsInArray = Seq.toArray contents
    let index = Array.findIndex(fun elem -> elem.Equals(row)) contentsInArray
    index

  let findParent (indexOfHead:int,numWhiteSpacesInChild:int) =   
    let contentsAboveChild = (contents |> Seq.toList).[0..indexOfHead]
    
    let potentialParents = contentsAboveChild |> List.filter(fun line -> let (databaseByThemes, data, folder) = tokenizeLineContent line
                                                                         if folder.Equals("folder") then
                                                                           let numWhiteSpaces = firstNonWhiteCharIndex databaseByThemes
                                                                           // printfn "PP [%s]: %i Child:%i" data numWhiteSpaces numWhiteSpacesInChild
                                                                           numWhiteSpaces < numWhiteSpacesInChild      
                                                                         else
                                                                           false)
    if (Seq.isEmpty potentialParents) then   
      "Not found"                       
    else
      let (databaseByThemes, data, folder) = tokenizeLineContent(Seq.last potentialParents)
      let numWhiteSpacesInParent = firstNonWhiteCharIndex databaseByThemes
      printfn "Parent Name Selected: %s[%i] Child: [%i]" databaseByThemes numWhiteSpacesInParent numWhiteSpacesInChild
      data
                        
  let getCodePrefix (code:string) =
    let lastIndexOfUnderscore = code.LastIndexOf("_")
    match lastIndexOfUnderscore with
      | -1 -> sprintf "Not found"
      | _ -> code.[0..lastIndexOfUnderscore-1]
  
  let getParentFolderName (indexOfHead:int,databaseByThemes:string, code:string) =
    let parentCode = getCodePrefix(code)
    if parentCode.Equals("Not found") then
      let numWhiteSpaces = firstNonWhiteCharIndex databaseByThemes
      printfn "code:%s numwhitespace: %i" databaseByThemes numWhiteSpaces
      let parentFolderName = findParent(indexOfHead,numWhiteSpaces)
      parentFolderName
    else
      parentCode
  
  let rec readLines (contentsList, root:Folder, rootFolders:List<Folder>, rootDatasets:List<Dataset>) =
    match contentsList with 
      | [] -> printfn "End Reading Contents"
      | head :: tail -> let (databaseByThemes, data, folder) = tokenizeLineContent head
                        let indexOfHead = findIndexOfRow head
                        let parentCode = getParentFolderName(indexOfHead,databaseByThemes,data)
                        match folder with
                          | "dataset" -> let newDataset = {Title = databaseByThemes; Code=data}
                                         match parentCode with
                                           | "Not found" -> printfn "Database prefix not found: Adding Dataset [%s] to %s" data root.Data; 
                                                            root.Datasets.Add(newDataset)  
                                           | _ -> let parentFolder = findFolder(root, parentCode)
                                                  match parentFolder.DatabaseByThemes with
                                                    | "databaseByThemes" -> printfn "Parent folder not found: Adding dataset [%s] to %s" data root.Data;
                                                                            root.Datasets.Add(newDataset)  
                                                    | _ -> printfn "Adding dataset [%s] to folder [%s]" data parentFolder.Data; 
                                                           parentFolder.Datasets.Add(newDataset)  
                          | "folder" -> let newFolder = { DatabaseByThemes=databaseByThemes; Data=data; 
                                          Folders=new List<Folder>(); 
                                          Datasets=new List<Dataset>()}
                                        match parentCode with
                                          | "Not found" -> printfn "Folder %s not found: Adding Folder [%s] added to %s" parentCode data root.Data; 
                                                           root.Folders.Add(newFolder)  
                                          | _ -> let parentFolder = findFolder(root, parentCode)
                                                 match parentFolder.DatabaseByThemes with
                                                   | "databaseByThemes" -> printfn "Parent folder not found: Adding Folder [%s] to %s" data root.Data;
                                                                           root.Folders.Add(newFolder)  
                                                   | _ -> printfn "Adding Folder [%s] to %s" data parentFolder.Data;
                                                          parentFolder.Folders.Add(newFolder)  
                          | _ -> printfn "Neither folder nor dataset"
                        readLines(tail, root, root.Folders, root.Datasets)
                                           
  let readTree =
    let rootLine = Seq.head contents
    let branchesLines = Seq.tail contents
    let (databaseByThemes, data, folder) = tokenizeLineContent rootLine
    let root = { DatabaseByThemes=databaseByThemes; Data=data; 
                 Folders = new List<Folder>(); 
                 Datasets= new List<Dataset>()} 
    readLines ((branchesLines |> Seq.toList), root, root.Folders, root.Datasets)
    printfn "Root: %A" root
    
    

    // root.Datasets |> Seq.iter(fun d -> printfn "Datasets: %A" d)
    // root.Folders |> Seq.iter(fun d -> printfn "Folders: %A" d)
    // let result = findFolder(root, "scitech")
    // printfn "folders: %A" result.Folders
    // printfn "datasets: %A" result.Datasets

    // let parent =findParent(12) 
    // printfn "Parent:\n %A" parent
    
    
// let rec findFolder2 (rootFolders:List<Folder>, folderCode:string) =
  //   match rootFolders with
  //     | [] -> None
  //     | head::tail -> //printfn "%s" head.DatabaseByThemes
  //                     if head.DatabaseByThemes.Equals(folderCode) then
  //                       head
  //                     else  
  //                       findFolder (head.Folders, folderCode)
  //                     findFolder (tail, folderCode) 
  
    
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
  
      