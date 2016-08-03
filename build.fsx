// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "packages/FAKE/tools/FakeLib.dll"
open Fake
open System
open System.IO
open FSharp.Data

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let fsiPath = "packages/FSharp.Compiler.Tools/tools/fsiAnyCpu.exe"
let fscPath = "packages/FSharp.Compiler.Tools/tools/fsc.exe"
let outPath = "bin/server.exe"

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
        info.Arguments <- "--load:src/run.fsx"
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

Target "deploy" (fun _ ->
  let wwwroot = "../wwwroot"
  ensureDirectory (wwwroot </> "bin")
  CopyRecursive "bin" (wwwroot </> "bin") false |> ignore
  CopyFile (wwwroot </> "web.config") "web.config"
)

"build" ==> "deploy"

RunTargetOrDefault "run"
