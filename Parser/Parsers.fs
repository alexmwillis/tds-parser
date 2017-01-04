module TdsParser.Parsers

    open System
    open FParsec
    open TdsParser.FParsecExtensions
    open TdsParser.Types
        
    let toVersion language version revision =
        { 
            Language = Language language
            Version = version 
            Revision = revision 
        }

    let toField version field name key content =
        { 
            FieldId = field
            Name = name
            Key = key
            Value = content
            Version = version 
        }

    let toItem version id database path parent name master template templatekey sharedFields versionedFields =
        {
            Version = version
            Id = id
            Database = database
            Path = path
            Parent = parent
            Name = name
            Master = master
            Template = template
            Templatekey = templatekey
            Fields = List.concat [sharedFields; (List.collect (fun v -> v) versionedFields)]
        }     

    let parseGuid =
            let guidRegex = @"^[{(]?[0-9A-Fa-f]{8}[-]?([0-9A-Fa-f]{4}[-]?){3}[0-9A-Fa-f]{12}[)}]?"
            regex guidRegex |>> Guid.Parse
    
    let parseKeyValue key pvalue = pstring key >>. pstring ": " >>. pvalue
    let parseKeyValueString key = parseKeyValue key (restOfLine true)
    let parseKeyValueInt key = parseKeyValue key pint32 .>> newline
    let parseKeyValueGuid key = parseKeyValue key parseGuid .>> newline
    
    let parseFieldContent = 
            (parseKeyValueInt "content-length" .>> newline) >>= fun contentLength -> (anyString contentLength .>> newline)
    
    let parseField version = 
            pstring "----field----" >>. newline >>. 
            pipe4 
                (parseKeyValueGuid "field") 
                (parseKeyValueString "name") 
                (parseKeyValueString "key") 
                parseFieldContent
                (toField version)

    let parseSharedField = parseField None

    let parseVersion = 
            pstring "----version----" >>. newline >>.
            pipe3
                (parseKeyValueString "language")
                (parseKeyValueInt "version")
                (parseKeyValueGuid "revision")
                toVersion
            .>> newline 

    let parseVersionedFields =
        parseVersion >>= (Some >> parseField >> many)

    let parseItem: Parser<_> = 
            pstring "----item----" >>. newline  >>.
            pipe11
                (parseKeyValueInt "version")
                (parseKeyValueGuid "id")
                (parseKeyValueString "database")
                (parseKeyValueString "path")
                (parseKeyValueGuid "parent")
                (parseKeyValueString "name")
                (parseKeyValueGuid "master")
                (parseKeyValueGuid "template")
                (parseKeyValueString "templatekey" .>> newline)
                (many1 parseSharedField)
                (many1 parseVersionedFields)
                toItem

    let runParser p str =
        match run p str with
        | Success(result, _, _)   -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg