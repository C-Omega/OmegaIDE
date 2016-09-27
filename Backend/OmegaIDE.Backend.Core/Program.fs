// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open OmegaIDE.FSharp
open OmegaIDE.FSharp.IPC
open OmegaIDE.FSharp.Modules
[<EntryPoint>]
let main argv = 
    let file = IPC()//ep anyv4 0)
    printfn "%A" <| String.concat " "[|string loopbackv4;string file.LocalEndpoint.Port; "config"|]
    let f = System.Diagnostics.Process.Start("OmegaIDE.Backend.File.exe",String.concat " "[|string file.LocalEndpoint.Address;string file.LocalEndpoint.Port; "config"|])
    match file.Receive() with |{parts = [String "bound to"; Bytes b]} -> _b_int32 b |> ep loopbackv4 |> file.Connect |_ -> ()
    file.Send{parts = [String "get"; String "highlighter"; String "fsharp"]}
    let h = match file.Receive() with |{parts = [String "got"; String "highlighter"; String "fsharp"; String v]} -> Highlighting.SyntaxHighlighter.OfString v |e ->failwithf "%A" e
    System.Console.ReadLine() + "\n" |> h.update |> printfn "%A"
    0 // return an integer exit code

