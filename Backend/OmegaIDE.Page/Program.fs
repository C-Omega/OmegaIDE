// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open OmegaIDE.FSharp
[<EntryPoint>]
let main argv = 
    let v = "@comment@//.*\n" |> Modules.Highlighting.SyntaxHighlighter.OfString
    v.update "//hi\n//hi again\n" |> printfn "%A"
    0 // return an integer exit code

