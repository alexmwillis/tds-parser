module TdsParser.Project

    open System.IO
    open TdsParser.Types
    open TdsParser.Parsers
    open TdsItems
    
    let getChildFiles (file:FileInfo) = 
        let pathOfChildren = Path.Combine(file.DirectoryName, Path.GetFileNameWithoutExtension(file.FullName))
        let directory = new DirectoryInfo(pathOfChildren)
        if directory.Exists
            then directory.GetFiles("*.item")
            else [||]     
    
    let parseItem (file:FileInfo) children = 
        let content = File.ReadAllText(file.FullName)
        runParser (parseItem children) content
    
    let rec getItem file = 
        let childFiles = getChildFiles file
        if childFiles.Length > 0 
            then parseItem file (childFiles |> Seq.map getItem) 
            else parseItem file Seq.empty

    let rec flattenItem item = 
        Seq.append (Seq.singleton item) (Seq.collect flattenItem item.Children) 

    let rec flattenItems items = 
        Seq.append items (Seq.collect flattenItem items) 

    let getProject (projectPath:string) =
        let fullProjectPath = 
            if projectPath.StartsWith("/") 
                then Path.Combine(__SOURCE_DIRECTORY__, projectPath) 
                else projectPath

        let children = getChildFiles (new FileInfo(fullProjectPath))
        if children.Length > 0
            then children 
                |> Seq.map getItem 
                |> flattenItems 
                |> Seq.groupBy (fun i -> i.Id)
                |> Map.ofSeq
                |> Map.map (fun id items -> 
                    match (List.ofSeq items) with
                    | x::xs when xs.IsEmpty -> x
                    | x::xs -> failwith (sprintf "duplicate item '%s'" x.Path)
                    | [] -> failwith "empty list")
            else failwith "project is empty"
    