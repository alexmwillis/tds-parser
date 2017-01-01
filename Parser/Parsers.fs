module Parsers

    open System
    open FParsec
    
    let pipe11 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 f =
        pipe3 (tuple4 p1 p2 p3 p4) (tuple4 p5 p6 p7 p8) (tuple3 p9 p10 p11)
            (fun (x1, x2, x3, x4) (x5, x6, x7, x8) (x9, x10, x11) -> f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)

    type Language = Language of string
    type FieldName = FieldName of string
            
    type Version = {
        version: Int32;
        language: Language;
        revision: Guid;
    }

    let toVersion = fun language version revision -> 
        { 
            language = Language language; 
            version = version; 
            revision = revision 
        }

    type Field = {
        fieldId: Guid; 
        name: string;
        key: string;
        value: string;
        version: option<Version>
    }

    let toField version = fun field name key content -> 
        { 
            fieldId = field; 
            name = name; 
            key = key; 
            value = content; 
            version = version 
        }

    type Item = {
        version: Int32;
        id: Guid;
        database: string; 
        path: string; 
        parent: Guid; 
        name: string; 
        master: Guid; 
        template: Guid; 
        templatekey: string;
        fields: List<Field>;
    }

    let toItem = fun version id database path parent name master template templatekey sharedFields versionedFields ->
        {
            version = version;
            id = id;
            database = database;
            path = path;
            parent = parent;
            name = name;
            master = master;
            template = template;
            templatekey = templatekey;
            fields = List.concat [sharedFields; (List.collect (fun v -> v) versionedFields)];
        }     

    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>

    let parseGuid: Parser<_> =
            let guidRegex = @"^[{(]?[0-9A-Fa-f]{8}[-]?([0-9A-Fa-f]{4}[-]?){3}[0-9A-Fa-f]{12}[)}]?"
            regex guidRegex |>> fun a -> Guid.Parse a
    
    let parseKeyValue key pvalue = pstring key >>. pstring ": " >>. pvalue
    let parseKeyValueString key = parseKeyValue key (restOfLine true)
    let parseKeyValueInt key = parseKeyValue key pint32 .>> newline
    let parseKeyValueGuid key = parseKeyValue key parseGuid .>> newline
    
    let parseFieldContent: Parser<_> = 
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
        parseVersion >>= fun version -> many (parseField (Some version))

    let parseItem = 
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
                (parseKeyValueString "templateKey")
                (many parseSharedField)
                (many parseVersionedFields)
                toItem
