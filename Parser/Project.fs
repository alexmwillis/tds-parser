module TdsParser.Project

    open System.IO
    open TdsParser.Types
    open TdsParser.Parsers
    
    let getItem (file:FileInfo) = 
        let content = File.ReadAllText(file.FullName)
        runParser parseItem content
    
    let getChildren (file:FileInfo) = 
        let pathOfChildren = Path.Combine(file.DirectoryName, Path.GetFileNameWithoutExtension(file.FullName))
        let directory = new DirectoryInfo(pathOfChildren)
        if directory.Exists
            then directory.GetFiles("*.item")
            else [||]     
    
    let rec getProjectTree file = 
        let children = getChildren file
        if children.Length > 0 
            then Branch(getItem file, children |> Seq.map getProjectTree)
            else Leaf(getItem file)

    let getProject projectPath =
        let fullProjectPath = Path.Combine(__SOURCE_DIRECTORY__, projectPath)
        let children = getChildren (new FileInfo(fullProjectPath))
        if children.Length > 0
            then Root(children |> Seq.map getProjectTree)
            else failwith "project is empty"