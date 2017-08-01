#if INTERACTIVE
#I "../../packages"
#r "System.Xml.Linq.dll"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "Suave/lib/net40/Suave.dll"
// #load "../serializer.fs"
#load "vocab.fs"
#load "domain.fs"
#else
module Services.Eurostat
#endif
#nowarn "1104"
open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Eurostat
open Eurostat.Domain
// open Services.Serializer

let geo = Serializer.getEUCountries