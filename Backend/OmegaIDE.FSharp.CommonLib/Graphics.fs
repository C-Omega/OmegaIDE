namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open System.Text.RegularExpressions
module Graphics = 
    type RGBA = 
        {red:byte;green:byte;blue:byte;alpha:byte}
        member x.ToByteArray() = [|x.red;x.green;x.blue;x.alpha|]
        member x.ToUInt32() = System.BitConverter.ToUInt32(x.ToByteArray(),16)
        override x.ToString() = System.Convert.ToString(x.ToUInt32())
        static member OfByteArray(b:byte[]) = {red=b.[0];green=b.[1];blue=b.[2];alpha=b.[3]}
        static member OfUInt32 (i:uint32) = System.BitConverter.GetBytes i |> Array.rev |> RGBA.OfByteArray
        static member OfString (s:string) = System.Convert.ToUInt32(s,16) |> RGBA.OfUInt32
        static member Red = {red=255uy;green=0uy;blue=0uy;alpha=0uy}
        static member Green = {red=0uy;green=255uy;blue=0uy;alpha=0uy}
        static member Blue = {red=0uy;green=0uy;blue=255uy;alpha=0uy}
        static member Black = {red=0uy;green=0uy;blue=0uy;alpha=0uy}
        static member White = {red=255uy;green=255uy;blue=255uy;alpha=0uy}
        static member (+) (a:RGBA,b) = {red = (a.red+b.red)/2uy;green = (a.green+b.green)/2uy;blue = (a.blue+b.blue)/2uy;alpha = (a.alpha+b.alpha)/2uy}
    [<System.FlagsAttribute>]
    type Style = 
        |WarnSquiggle   =   0b00000001uy
        |ErrorSquiggle  =   0b00000010uy
        |Bold           =   0b00000100uy
        |Italics        =   0b00001000uy
        |Underline      =   0b00010000uy
    type KeywordType = 
        |Normal
        |MajorKeyword
        |MinorKeyword
        |Literal
        |LiteralString
        |Comment
        |Preprocessor//   =   5uy
        |Type
    type DescribedString = {s:string; style : Style; keywordtype : KeywordType}
    type GraphicString   = {s:string; foreground : RGBA; background : RGBA; style : Style}