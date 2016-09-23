namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
[<AutoOpen>]
module Core =
    let checksum (b:byte[]) = using (new System.Security.Cryptography.SHA512Managed()) (fun i -> i.ComputeHash b)
    let tobase64 = System.Convert.ToBase64String
    let ofbase64 = System.Convert.FromBase64String
    let utf8 = System.Text.Encoding.UTF8
    let tostring = utf8.GetString
    let ofstring(s:string) = utf8.GetBytes s
    let remove_whitespace s = System.Text.RegularExpressions.Regex.Replace(s,"[ \n\t]","")
module ProjectFile = 
    type ProjectFileNode = 
        {location : string; language : string; platforms : string[]; buildmode:string}//checksum : byte[]}
        override x.ToString() = "@node@"+x.location+":"+x.language+"?"+String.concat "," x.platforms+"!"+x.buildmode //+"?"+(tobase64 x.checksum)
        //static member Checksum x = 
            //{location = x.location; language = x.language; checksum = "@"+x.location+":"+x.language |> ofstring |> checksum}
        static member OfString (s:string) = 
            let i = s.IndexOf('@',1) // after @node@ tag
            let a = s.LastIndexOf ":"
            let b = s.LastIndexOf "?"
            let c = s.LastIndexOf "!"
            {
                location = s.[i+1..a-1];
                language = s.[a+1..b-1];
                platforms = s.[b+1..c-1].ToLower().Split([|','|],System.StringSplitOptions.RemoveEmptyEntries);
                buildmode = s.ToLower().[c+1..];
            }
    type ProjectFile = 
        {
            name : string
            nodes : ProjectFileNode[]
        }
        override x.ToString() = "@name@"+x.name+";\n"+(Array.fold (fun acc elem -> acc + "\n" + string elem + ";") "" x.nodes)
        static member OfString (s:string) = 
            //this removes any form of newline (CRLF or LF), and elimits it by semicolons
            let s = System.Text.RegularExpressions.Regex.Replace(s,"[\r\n]","").Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            //a recursive parser
            let rec parse = function
                |[],name,nodes -> name,nodes
                |a::b,name,nodes -> 
                    //get the type
                    let x,y = (a:string).IndexOf '@', a.IndexOfAny([|'@';' '|],1)
                    //get the value
                    let z = a.[a.IndexOf('@',1)+1..]
                    match a.[x+1..y-1].ToLower() with
                    |"name" -> parse(b,z,nodes)
                    |"node" -> parse(b,name,ProjectFileNode.OfString z :: nodes)
                    |s -> raise (System.ArgumentException(s))
            let name,nodes = parse (s,"",[])
            {name = name; nodes = Array.ofList nodes}
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
        static member Red = {red=0uy;green=0uy;blue=255uy;alpha=0uy}
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
        |Normal of string
        |MajorKeyword of string
        |MinorKeyword of string
        |Literal of string
        |Comment of string * string
        |Preprocessor//   =   5uy
    type DescribedString = {s:string; style : Style; keywordtype : KeywordType}
    type GraphicString   = {s:string; foreground : RGBA; background : RGBA; style : Style}
module Modules =
    open Graphics
    module Highlighting =
        type SyntaxHighlighter = 
            {update : string -> DescribedString[]}
            static member OfString(s:string) =
                let s = System.Text.RegularExpressions.Regex.Replace(s,"[\r\n]","").Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
                let rec parse = function
                    |[],c -> c
                    |a::b,c -> 
                        //get the type
                        let x,y = (a:string).IndexOf '@', a.IndexOfAny([|'@';' '|],1)
                        let i = a.IndexOf('@',1)
                        //get the value
                        let z = a.[i+1..]
                        match a.[x+1..y-1].ToLower() with
                        |"majorkeyword" -> parse(b,(MajorKeyword,z)::c)
                        |"minorkeyword" -> parse(b,(MinorKeyword,z)::c)
                        |"comment" -> 
                            let delim = if y + 1 = i then '@' else x.[y+1] // y + 1 = i when no tag args
                            let d = z.IndexOf(delim)
                            parse(b,Comment(z.[..d-1],z.[d+1..]) :: nodes)
                        |s -> raise (System.ArgumentException(s))
                let name,nodes = parse (s,"",[])