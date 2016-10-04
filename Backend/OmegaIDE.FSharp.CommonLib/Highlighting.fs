namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open System.Text.RegularExpressions
module Highlighting =
    open Graphics
    type SyntaxHighlighter = 
        {update : string -> (KeywordType * string)list}
        static member OfString(s:string) =
            //let s = regex(s,"[\r\n]","").Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            let rec parse = function
                |[],c -> c
                |a::b,c -> 
                    //get the type
                    let x = (a:string).IndexOf '@'
                    let y = a.IndexOf('@',x+1)
                    printfn "%A" (x,y)
                    //get the value
                    let z = regex(a.[y+1..])
                    match a.[x+1..y-1].ToLower() with
                    |"majorkeyword" ->  parse(b,(MajorKeyword, z) :: c)
                    |"minorkeyword" ->  parse(b,(MinorKeyword, z) :: c)
                    |"literal" ->       parse(b,(Literal, z) :: c)
                    |"literalstring" -> parse(b,(LiteralString, z) :: c)
                    |"comment" ->       parse(b,(Comment, z) :: c)
                    |"type"          -> parse(b,(Type, z) :: c)
                    |""              -> parse(b,c)
                    |s -> raise (System.ArgumentException(s))
            let t = parse(s.Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray,[]) |> Array.ofList
            let c = Array.Parallel.choose (function |Comment, r -> Some r |_ -> None) t
            let l = Array.Parallel.choose (function |LiteralString, r -> Some r |_ -> None) t
            let r = Array.Parallel.choose (function |Comment,_ -> None |a,b -> Some(a,b)) t
            let dematch(a:Match) = a.Index,a.Length
            {update = fun str -> 
                let s,pass1 = 
                    Array.fold (fun (s:string,stuff:(KeywordType*(int*int)) list) (r:Regex) -> 
                        r.Matches(s) 
                        |> Seq.cast<Match>
                        |> Seq.fold(fun (acc:string,stuff:(KeywordType*_) list) (r:Match) ->
                            let start = r.Index-1
                            let stop = r.Index+r.Length
                            (
                                (if start < 0 then "" else acc.[..r.Index-1]) +
                                String.replicate (r.Length-1) "\u0002" +
                                "\u0001" +
                                (if stop >= acc.Length then "" else acc.[stop..])
                            ),(Comment,dematch r)::stuff) (s,stuff)
                        ) (str,[]) c
                let s,pass2 =
                    Array.fold (fun (s:string,stuff:(KeywordType*(int*int)) list) (r:Regex) -> 
                        r.Matches(s) 
                        |> Seq.cast<Match>
                        |> Seq.fold(fun (acc:string,stuff:(KeywordType*_) list) (r:Match) ->
                            let start = r.Index-1
                            let stop = r.Index+r.Length
                            (
                                (if start < 0 then "" else acc.[..r.Index-1]) +
                                String.replicate (r.Length-1) "\u0002" +
                                "\u0001" +
                                (if stop >= acc.Length then "" else acc.[stop+0..])
                            ),(Comment,dematch r)::stuff) (s,stuff) 
                        ) (s,pass1) c
                let s,pass3 = 
                    Array.fold (fun (s:string,stuff:(KeywordType*(int*int)) list) (kt,r:Regex) -> 
                        r.Matches(s) 
                        |> Seq.cast<Match>
                        |> Seq.fold(fun (acc:string,stuff:(KeywordType*(int*int)) list) (r:Match) ->
                            let start = r.Index-1
                            let stop = r.Index+r.Length
                            (
                                (if start < 0 then "" else acc.[..start]) +
                                String.replicate (r.Length-1) "\u0002" +
                                "\u0001" +
                                (if stop >= acc.Length then "" else acc.[stop..])
                            ),(kt,dematch r)::stuff) (s,stuff)
                        ) (s,pass2) r
                let pass4 = 
                    let rec inner = function
                        |i,n when i = s.Length -> n
                        |i,n when i = s.Length - 1 -> if s.[i] = '\u0001' then n else (Normal,(i,1))::n
                        |i,n ->
                            if s.[i] = '\u0001' then inner(i+1,n)
                            elif s.[i] = '\u0002' then let j = s.IndexOf('\u0001',i) in inner(j+1,n)
                            else 
                                let j = s.IndexOfAny([|'\u0001';'\u0002'|],i)
                                if j = -1 then (Normal,(i,s.Length-i))::n
                                else let s' = s.[i..j-1] in inner(j,(Normal,(i,s'.Length))::n)
                           
                    inner(0,[])
                List.map(fun (kt,(i,l)) -> i,(kt,str.[i..i+l-1])) (pass3@pass4)
                |> List.sortBy fst
                |> List.map snd
            }