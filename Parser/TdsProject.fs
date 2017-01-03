module TdsProject

    open System.IO

    type Item = {
        content: string
    }

    type Tree = 
        | Root of Tree seq
        | Leaf of Item
        | Branch of Item * Tree seq
        
    let getChildren (directory:DirectoryInfo) = 
        if directory.Exists
            then directory.GetFiles("*.item")
            else Array.empty

    let getChildren (file:FileInfo) = 
        let path = Path.Combine(file.DirectoryName, Path.GetFileNameWithoutExtension(file.FullName))
        getChildren (new DirectoryInfo(path))            
            
    let getItem (file:FileInfo) =
        { content = File.ReadAllText(file.FullName) }
            
    let getProject projectPath =
        Root = map getChildren projectPath
        
       // let fullProjectPath = Path.Combine(__SOURCE_DIRECTORY__, projectPath)

//        if !Directory.Exists(fullProjectPath) 
//        then failwith (sprintf "invalid project path %s" fullProjectPath)

    let getProjectItems