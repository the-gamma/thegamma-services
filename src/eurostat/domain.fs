namespace Eurostat.Domain

open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Eurostat
open Eurostat.Vocabulary

// type Country = 
//   { Name : string
//     Status : string 
//   }

module Serializer =
  let getEUCountries =    
    let dict = Vocabulary.getCountries
    printfn "Dict: %A" dict
    dict