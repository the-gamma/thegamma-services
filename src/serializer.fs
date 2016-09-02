module Services.Serializer
open Newtonsoft.Json
open FSharp.Data

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

let serializer = JsonSerializer.Create()

let toJson value = 
  let sb = System.Text.StringBuilder()
  use tw = new System.IO.StringWriter(sb)
  serializer.Serialize(tw, value)
  sb.ToString() 

let formatPairSeq kf data =
  let json = 
    data 
    |> Seq.map (fun (k, v) -> JsonValue.Array [| kf k; JsonValue.Float v |])
    |> Array.ofSeq
    |> JsonValue.Array
  json.ToString()
