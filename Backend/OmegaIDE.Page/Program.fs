// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open OmegaIDE.FSharp
open OmegaIDE.FSharp.IPC
open OmegaIDE.FSharp.ProjectFile
open OmegaIDE.FSharp.Modules.Highlighting
open OmegaIDE.FSharp.Config
open OmegaIDE.FSharp.Tree
open System.IO
let a = ep loopbackv4 9011
let b = ep loopbackv4 9012
[<EntryPoint>]
let main argv = 
    {conf = [Branch("branch",[Leaf("leaf","value")])]} |> side (printfn "%A") |> string |> side (printfn "%A") |> Config.OfString |> printfn "%A"
    (*
    let ipc = IPC(ep (ip argv.[0]) 0, ep (ip argv.[0]) (int argv.[1]))
    let config = File.ReadAllText argv.[2]
    while true do 
        match ipc.Receive() with
        |{parts = String "load"::String what::String path::[]} ->
            match what with
            |"project" -> 

            |"highlighter" -> SyntaxHighlighter.OfString

    ipc1.Send  {parts = [Bytes[|0uy|];String "hi";KeyValue("foo","bar")]}
    //let v = "@comment@\(\*.*\*\);\n@comment@//.*\\n" |> Modules.Highlighting.SyntaxHighlighter.OfString
    //v.update "normal(*comment*)\n//comment 2\n" |> printfn %A
    ipc2.Receive() |> printfn %A
    *)
    0 // return an integer exit code

