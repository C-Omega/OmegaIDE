open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open OmegaIDE.FSharp
open OmegaIDE.FSharp.IPC
open OmegaIDE.FSharp.Highlighting
type Colour = System.ConsoleColor
let cprint c s = 
    System.Console.ForegroundColor <- c
    printf "%s" s
    System.Console.ResetColor()
[<EntryPoint>]
let main argv = 
    let file = IPC()
    let f = System.Diagnostics.Process.Start("OmegaIDE.Backend.File.exe",String.concat " "[|string loopbackv4;string file.LocalEndpoint.Port; "config"|])
    match file.Receive() with |{parts = [String "bound to"; Bytes b]} -> b |> _b_int32 |> ep loopbackv4 |> file.Connect |_ -> ()
    let compiler = IPC()
    let c = System.Diagnostics.Process.Start("OmegaIDE.Backend.Compiler.exe",String.concat " "[|string loopbackv4;string compiler.LocalEndpoint.Port; "config"|])
    match compiler.Receive() with |{parts = [String "bound to"; Bytes b]} -> b |> _b_int32 |> ep loopbackv4 |> compiler.Connect |_ -> ()
    compiler.Send {parts = [String "load"   ; String <| System.IO.File.ReadAllText "../../../../Compilers/fsharp"]}
    compiler.Receive() |> printfn "%A"
    compiler.Send {parts = [String "compile"; String <| System.IO.File.ReadAllText "proj"]}
    compiler.Receive() |> printfn "%A"
    while true do ()
    0
    (*
    file.Send{parts = [String "get"; String "highlighter"; String "fsharp"]}
    let h = match file.Receive() with |{parts = [String "got"; String "highlighter"; String "fsharp"; String v]} -> Highlighting.SyntaxHighlighter.OfString v |e ->failwithf "%A" e
    printfn "%O" h
    System.IO.File.ReadAllText("../../../OmegaIDE.FSharp.CommonLib/Component1.fs") |> h.update |> 
    Seq.iter(function
        |MajorKeyword ,s -> cprint Colour.DarkBlue s
        |MinorKeyword ,s -> cprint Colour.Blue s
        |Literal      ,s
        |LiteralString,s -> cprint Colour.DarkYellow s
        |Comment      ,s -> cprint Colour.Cyan s
        |Normal       ,s -> cprint Colour.Black s
        |Type         ,s -> cprint Colour.DarkGreen s
        |Preprocessor ,s -> cprint Colour.DarkCyan s
    )
    System.Threading.Thread.Sleep -1
    System.Console.ReadLine()
    0 // return an integer exit code
    *)
