namespace Eurostat
open System
open System.IO
open System.IO.Compression
open FSharp.Data
open System.Collections.Generic



//http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Faact_ali01.tsv.gz
//http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&downfile=data%2Faact_ali01.tsv.gz
//http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Frd_p_persocc.tsv.gz

module Requests =

  let fileRoot = "/Users/myong/Documents/workspace/thegamma-services/data/eurostat/data"
  let eurostatUrl datasetName = 
    let root = "http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/"
    sprintf "%s%s.tsv.gz" root datasetName

  // let ExtractAll =
  //   printfn "%A" (Directory.EnumerateFiles(fileRoot, "*.gz"))
  //   Directory.EnumerateFiles(fileRoot, "*.gz")
  //   |> Seq.iter (fun zipPath -> printfn "Uncompressing %s" (Path.GetFileName(zipPath))
  //                               let archive = ZipFile.Open(zipPath, ZipArchiveMode.Read)
  //                               archive.ExtractToDirectory(targetDir))
  
  let getDataset datasetName =
    let url = eurostatUrl datasetName
    let response = Http.Request(url, httpMethod="GET", headers = ["Accept", "text/html,application/xhtml+xml,application/xml";"Accept-Encoding", "gzip, deflate";"User-Agent", "Mozilla/5.0 (Windows NT 6.2; WOW64; rv:19.0) Gecko/20100101 Firefox/19.0"])
    match response.Body with
    | Text text ->
        printfn "This was text"
    | Binary bytes ->
        printfn "Got %d bytes of binary content" bytes.Length
        File.WriteAllBytes(Path.Combine(fileRoot, sprintf "%s.csv.gz" datasetName), bytes)
    // ExtractAll
  
