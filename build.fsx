// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "packages/FAKE/tools/FakeLib.dll"
#load "packages/FSharp.Azure.StorageTypeProvider/StorageTypeProvider.fsx"

open Fake
open System
open System.IO
open FSharp.Data

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let fsiPath = "packages/FSharp.Compiler.Tools/tools/fsiAnyCpu.exe"

// --------------------------------------------------------------------------------------
// For deployed run - compile as an executable
// --------------------------------------------------------------------------------------

Target "clean" (fun _ ->
  CleanDirs ["bin"]
)

Target "build" (fun _ ->
  [ "thegamma-services.sln" ]
  |> MSBuildRelease "" "Rebuild"
  |> Log ""
)

"clean" ==> "build"

// --------------------------------------------------------------------------------------
// For local run - automatically reloads scripts
// --------------------------------------------------------------------------------------

let startServers () = 
  ExecProcessWithLambdas
    (fun info -> 
        info.FileName <- System.IO.Path.GetFullPath fsiPath
        info.Arguments <- "--load:src/debug.fsx"
        info.WorkingDirectory <- __SOURCE_DIRECTORY__)
    TimeSpan.MaxValue false ignore ignore 

Target "start" (fun _ ->
  async { return startServers() } 
  |> Async.Ignore
  |> Async.Start
  
  let mutable started = false
  while not started do
    try
      use wc = new System.Net.WebClient()
      started <- wc.DownloadString("http://localhost:10042/minimal").Contains("London")
    with _ ->
      System.Threading.Thread.Sleep(1000)
      printfn "Waiting for servers to start...."
  traceImportant "Servers started...."
)

Target "run" (fun _ ->
  traceImportant "Press any key to stop!"
  Console.ReadKey() |> ignore
)

"start" ==> "run"

// --------------------------------------------------------------------------------------
// Azure - deploy copies the binary to wwwroot/bin
// --------------------------------------------------------------------------------------

open Microsoft.WindowsAzure.Storage

let downloadWorldBank cache =
  let conn = Environment.GetEnvironmentVariable("CUSTOMCONNSTR_THEGAMMADATA_STORAGE")
  let conn = if conn = null then File.ReadAllText(__SOURCE_DIRECTORY__ + "/config.txt") else conn
  let account = CloudStorageAccount.Parse(conn)
  let wb = account.CreateCloudBlobClient().GetContainerReference("worldbank")
  let files = ["countries.json";"indicators.json";"years.json";"worldbank.dat"]
  for file in files do
    let blob = wb.GetBlockBlobReference(file)
    blob.DownloadToFile(cache </> file, FileMode.Create)

let newName prefix f = 
  Seq.initInfinite (sprintf "%s_%d" prefix) |> Seq.skipWhile (f >> not) |> Seq.head

Target "deploy" (fun _ ->
  // Pick a subfolder that does not exist
  let wwwroot = "../wwwroot"
  let subdir = newName "deploy" (fun sub -> not (Directory.Exists(wwwroot </> sub)))
  
  // Deploy everything into new empty folder
  let deployroot = wwwroot </> subdir
  CleanDir deployroot
  CleanDir (deployroot </> "bin")
  CleanDir (deployroot </> "data")
  CleanDir (deployroot </> "cache")
  CopyRecursive "bin" (deployroot </> "bin") false |> ignore
  CopyRecursive "data" (deployroot </> "data") false |> ignore
  let config = File.ReadAllText("web.config").Replace("%DEPLOY_SUBDIRECTORY%", subdir)
  File.WriteAllText(wwwroot </> "web.config", config)

  // Download WorldBank data to cache
  downloadWorldBank (deployroot </> "cache")

  // Try to delete previous folders, but ignore failures
  for dir in Directory.GetDirectories(wwwroot) do
    if Path.GetFileName(dir) <> subdir then 
      try CleanDir dir; DeleteDir dir with _ -> ()
)

"build" ==> "deploy"

RunTargetOrDefault "run"
