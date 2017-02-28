open System
open System.Text.RegularExpressions

let hexStringToNumberString str =
    Regex.Replace (str, "\W", "")
    |> Seq.windowed 2
    |> Seq.mapi (fun i n -> (i,n))
    |> Seq.filter (fun (i,n)-> i%2=0)
    |> Seq.map (fun (_,a) -> new string(a))
    |> Seq.map (fun str -> UInt32.Parse(str,System.Globalization.NumberStyles.AllowHexSpecifier))
    |> Seq.map (fun num -> num.ToString ())
    |> Seq.windowed 3
    |> Seq.mapi (fun i n -> (i,n))
    |> Seq.filter (fun (i,n)-> i%3=0)
    |> Seq.map (fun (_,a) -> 
        a 
        |> Array.map (fun numStr -> numStr.PadRight(4))
        |> Array.reduce (+))
    |> Seq.windowed 8
    |> Seq.mapi (fun i n -> (i,n))
    |> Seq.filter (fun (i,n)-> i%8=0)
    |> Seq.map (fun (_,a) -> 
        a 
        |> Array.map (fun numStr -> numStr.PadRight(16))
        |> Array.reduce (+))
    |> Seq.map (fun str -> str.Trim())
    |> Seq.reduce (fun acc str -> acc + "\n" + str)