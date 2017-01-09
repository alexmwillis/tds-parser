module TdsParser.Project

    open System.IO
    open TdsParser.Types
    open TdsParser.Parsers
    
    let internal getChildFiles (file:FileInfo) = 
        let pathOfChildren = Path.Combine(file.DirectoryName, Path.GetFileNameWithoutExtension(file.FullName))
        let directory = new DirectoryInfo(pathOfChildren)
        if directory.Exists
            then directory.GetFiles("*.item")
            else [||]     
    
    let internal parseItem (file:FileInfo) children = 
        let content = File.ReadAllText(file.FullName)
        runParser (parseItem children) content
    
    let rec internal getItem file = 
        let childFiles = getChildFiles file
        if childFiles.Length > 0 
            then parseItem file (childFiles |> Seq.map getItem) 
            else parseItem file Seq.empty

    let public GetProject (projectPath:string) =
        let children = getChildFiles (new FileInfo(projectPath))
        if children.Length > 0
            then children 
                |> Seq.map getItem 
            else failwith "project is empty"
    