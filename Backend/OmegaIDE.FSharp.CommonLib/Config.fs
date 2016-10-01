namespace OmegaIDE.FSharp
open C_Omega
open C_Omega.ArraySliceImprovement
open C_Omega.Helpers
open System.Text.RegularExpressions
module Config =
    open KVTree
    type Config = 
        {
            conf : KVTree list
        }
        member x.Item i = List.find(function |Branch(l,_) when l = i -> true |_ -> false) x.conf
        override x.ToString() =
            let rec inner i j = 
                let v = String.replicate i "\t"
                List.map (function 
                    |Root   l    -> List.fold(fun acc elem -> acc+"\n"+string elem) "" l
                    |Branch(l,t) -> String.concat "" [|v; "{;"; l; ";\n"; inner (i+1) t; "\n"; v; "};"|]
                    |Leaf  (a,b) -> v + "@" + a + "@" + b + ";"
                ) j
                |> String.concat "\n"
            inner 0 x.conf
        static member OfString(s:string) =
            //recursive regex
            let rec inner : string list * _ list -> string list * _ list = function
                |[],acc -> [],acc
                |a::n::l,acc when a.Contains("{") -> let a,b = inner(l,[]) in inner(a,Branch(n,b)::acc)
                |a::l,acc when a.Contains("}")-> l,(List.rev acc)
                |v::l,acc -> let j = regex(@"@(.*)@(.*)").Match(v)|> getgroups |> Array.ofSeq in inner(l,Leaf(j.[0],j.[1])::acc)
            {conf = inner((regexremove @"\n|\r|\t|    " s).Split([|';'|],System.StringSplitOptions.RemoveEmptyEntries)|>List.ofArray,[]) |> snd}