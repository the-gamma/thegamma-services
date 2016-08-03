module Services.Main

// --------------------------------------------------------------------------------------
// Entry-point for a real compiled server
// --------------------------------------------------------------------------------------

open Suave
open Suave.Filters
open Suave.Operators
open System

/// List of available servers - corresponds to folders
let servers = 
  [ "adventure", Services.Adventure.app
    "minimal", Services.Minimal.app
    "olympics", Services.Olympics.app
    "pivot", Services.Pivot.app
    "worldbank", Services.WorldBank.app ] |> dict
  
/// Drop the <s> part from http://localhost:123/<s>/something
let dropPrefix part ctx = 
  let u = ctx.request.url
  let local = 
    match List.ofArray (u.LocalPath.Substring(1).Split('/')) with
    | _::rest -> String.concat "/" rest
    | [] -> ""
  let url = System.Uri(u.Scheme + "://" + u.Authority + "/" + local)
  { ctx with request = { ctx.request with url = url }} |> part

// Server that serves pages from the dictionary
let handlePage s =
  match servers.TryGetValue(s) with
  | true, part -> dropPrefix part
  | _ -> RequestErrors.NOT_FOUND "Page not found"

let app =
  Writers.setHeader  "Access-Control-Allow-Origin" "*"
  >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type,x-cookie"
  >=> choose [ Filters.OPTIONS >=> Successful.OK "CORS approved"
               Filters.pathScan "/%s/%s" (fst >> handlePage)  
               Filters.pathScan "/%s" handlePage ]

let serverConfig =
  let portCmd = 
    System.Environment.GetCommandLineArgs() |> Seq.tryPick (fun s ->
      if s.StartsWith("port=") then Some(int(s.Substring("port=".Length)))
      else None )
  let portEnv = 
    match Int32.TryParse(Environment.GetEnvironmentVariable "HTTP_PLATFORM_PORT") with
    | true, port -> Some port
    | _ -> None
  let port = 
    match portCmd, portEnv with
    | Some p, _ | _, Some p -> p
    | _ -> failwith "No port specified"

  { Web.defaultConfig with
      homeFolder = Some __SOURCE_DIRECTORY__
      logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Info
      bindings = [ HttpBinding.mkSimple HTTP "127.0.0.1" port ] }

Web.startWebServer serverConfig app
