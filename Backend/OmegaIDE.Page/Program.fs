// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open OmegaIDE.FSharp
[<EntryPoint>]
let main argv = 
    let v = "@comment@\(\*.*\*\);\n@comment@//.*\\n" |> Modules.Highlighting.SyntaxHighlighter.OfString
    v.update "normal(*comment*)\n//comment 2\n" |> printfn "%A"
    0 // return an integer exit code

