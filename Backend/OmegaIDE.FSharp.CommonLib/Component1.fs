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
        {location : string; language : string; platform : string}//checksum : byte[]}
        override x.ToString() = "@node@"+x.location+":"+x.language+"?"+x.platform//+"?"+(tobase64 x.checksum)
        //static member Checksum x = 
            //{location = x.location; language = x.language; checksum = "@"+x.location+":"+x.language |> ofstring |> checksum}
        static member OfString (s:string) = 
            let i = s.LastIndexOf "@"
            let a = s.LastIndexOf ":"
            let b = s.LastIndexOf "?"
            {location = s.[i+1..a-1];language = s.[a+1..b-1];platform = s.[b+1..]}// checksum = s.[b+1..] |> ofbase64}
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
                    let x,y = (a:string).IndexOf "@", a.LastIndexOf "@"
                    //get the value
                    let z = a.[y+1..]
                    match a.[x+1..y-1].ToLower() with
                    |"name" -> parse(b,z,nodes)
                    |"node" -> parse(b,name,ProjectFileNode.OfString z :: nodes)
                    |s -> raise (System.ArgumentException(s))
            let name,nodes = parse (s,"",[])
            {name = name; nodes = Array.ofList nodes}