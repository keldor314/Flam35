module FormatHelper

open System
open System.Text.RegularExpressions


let partitionSeq width sequence =
  sequence
  |> Seq.windowed width
  |> Seq.mapi (fun i n -> (i,n))
  |> Seq.filter (fun (i,n) -> i%width = 0)
  |> Seq.map (fun (_,a) -> a)

let hexStringToNumberString str =
    Regex.Replace (str, "\W", "")
    |> partitionSeq 2
    |> Seq.map (fun a -> new string(a))
    |> Seq.map (fun str -> UInt32.Parse(str,System.Globalization.NumberStyles.AllowHexSpecifier))
    |> Seq.map (fun num -> num.ToString ())
    |> partitionSeq 3
    |> Seq.map (fun a -> 
        a 
        |> Array.map (fun numStr -> numStr.PadRight(4))
        |> Array.reduce (+))
    |> partitionSeq 8
    |> Seq.map (fun a -> 
        a 
        |> Array.map (fun numStr -> numStr.PadRight(16))
        |> Array.reduce (+))
    |> Seq.map (fun str -> str.Trim())
    |> Seq.reduce (fun acc str -> acc + "\n" + str)

