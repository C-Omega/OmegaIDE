namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
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
        static member Blue = {red=0uy;green=0uy;blue=255uy;alpha=0uy}
        static member Black = {red=0uy;green=0uy;blue=0uy;alpha=0uy}
        static member White = {red=255uy;green=255uy;blue=255uy;alpha=0uy}
        static member (+) (a:RGBA,b) = {red = (a.red+b.red)/2uy;green = (a.green+b.green)/2uy;blue = (a.blue+b.blue)/2uy;alpha = (a.alpha+b.alpha)/2uy}
module Modules =
    open System.Text.RegularExpressions
    open Graphics
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
    type DescribedString = {s:string; style : Style; keywordtype : KeywordType}
    type GraphicString   = {s:string; foreground : RGBA; background : RGBA; style : Style}
    module Highlighting =
        type SyntaxHighlighter = 
            {update : string -> (KeywordType * string)[]}
            static member OfString(s:string) =
                let s = System.Text.RegularExpressions.Regex.Replace(s,"[\r\n]","").Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
                let rec parse = function
                    |[],c -> c
                    |a::b,c -> 
                        //get the type
                        let x,y = (a:string).IndexOf '@', a.IndexOfAny([|'@';' '|],1)
                        let i = a.IndexOf('@',1)
                        //get the value
                        let z = Regex(a.[i+1..])
                        match a.[x+1..y-1].ToLower() with
                        |"majorkeyword" ->  parse(b,(MajorKeyword, z) :: c)
                        |"minorkeyword" ->  parse(b,(MinorKeyword, z) :: c)
                        |"literal" ->       parse(b,(Literal, z) :: c)
                        |"literalstring" -> parse(b,(LiteralString, z) :: c)
                        |"comment" ->       parse(b,(Comment, z) :: c)
                        |s -> raise (System.ArgumentException(s))
                let t = parse(s,[]) |> Array.ofList
                let c = Array.Parallel.choose (function |Comment, r -> Some r |_ -> None) t
                let l = Array.Parallel.choose (function |Literal, r -> Some r |_ -> None) t
                let r = Array.Parallel.choose (function |Comment,_ |Literal, _ -> None |a,b -> Some(a,b)) t
                {update = fun s -> 
                    let comments = 
                        c 
                        |> Array.Parallel.map (fun (r:Regex) -> r.Matches(s) |> Seq.cast<Match> |> Array.ofSeq) 
                        |> Array.concat
                    //we replace all the tokens with 1, so that everything keeps its place
                    let noncomments = Array.fold(fun (acc:string) (r:Match) -> 
                        let start = r.Index-1
                        let stop = r.Index+r.Length
                        (if start < 0 then "" else acc.[..r.Index-1]) + String.replicate r.Length "\u0001" + (if stop >= acc.Length then "" else acc.[stop..])) s comments
                    let literals = 
                        l 
                        |> Array.Parallel.map (fun (r:Regex) -> r.Matches(noncomments) |> Seq.cast<Match> |> Array.ofSeq)
                        |> Array.concat
                    let rest = Array.fold(fun (acc:string) (r:Match) -> acc.[..r.Index-1] + String.replicate r.Length "\u0001" + acc.[r.Index+r.Length..]) noncomments literals
                    let final = Array.Parallel.map (fun (t,r:Regex) -> (r.Matches(rest) |> Seq.cast<Match> |> Array.ofSeq),t) r
                    let x,y = 
                        Array.append [|comments,Comment;literals,Literal|] (final)
                        |> Array.Parallel.map (fun (i,j) -> Array.Parallel.map(fun k -> k,j) i)
                        |> Array.concat
                        |> Array.fold(fun (i:_ list,acc:string) (r:Match,k:KeywordType) -> 
                            let start = r.Index-1
                            let stop = r.Index+r.Length
                            //r.Index::i,((if start < 0 then "" else acc.[..r.Index-1]) + String.replicate r.Length "\u0001" + (if stop >= acc.Length then "" else acc.[stop..]))
                            (r, acc.[r.Index..stop-1])::i,
                            ((if start < 0 then "" else acc.[..r.Index-1]) + String.replicate r.Length "\u0001" + (if stop >= acc.Length then "" else acc.[stop..]))
                        ) ([],s)
                        |> function |i,j -> List.rev i,j
                    let replace (s:string) p (a:string) = s.[..p] + a + s.[p+a.Length-1..]
                    //nonfinal
                    Array.append [|comments,Comment;literals,LiteralString|] final
                    |> Array.map (fun (r,t) -> Array.map(fun (r:Match) -> t,s.[r.Index..r.Index+r.Length-1]) r)
                    |> Array.concat
                    // |> Array.groupBy snd
                }
                    //let comments = Array.Parallel.map(fun (r:Regex) -> r.Matches(s)) |> Array.concat
                    //In pass 1, we remove any comments, and store them for later

                    (*
                            match List.tryFind(fun (a,_) -> v.StartsWith(a)) with
                            |None -> pass1(comments,v.[1..])
                            |Some(a,b) -> 
                                let j = v.IndexOf(b)
                                if j = -1 then None else
                                pass1((v.[..v],i,j)::comments)
                    //In pass 2, we remove any string literals
                    let rec pass2 = function
                        |lits,_,"" -> Some lits //return when the char list is empty
                        |comments,i,v ->
                            match List.tryFind(fun (a,_) -> v.StartsWith(a)) with
                            |None -> pass1(comments,v.[1..])
                            |Some(a,b) -> 
                                let j = v.IndexOf(b)
                                if j = -1 then None else
                                pass1((v.[..v],i,j)::lits)
                    //With the comments and string literals out, we can highlight the important parts

                    ()*)