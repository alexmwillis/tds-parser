module TdsParser.Types

    open System
    open FParsec

    type Language = Language of string
    type FieldName = FieldName of string
            
    type Version = {
        Version: Int32
        Language: Language
        Revision: Guid
    }

    type Field = {
        FieldId: Guid
        Name: string
        Key: string
        Value: string
        Version: option<Version>
    }

    type Item = {
        Version: Int32
        Id: Guid
        Database: string
        Path: string
        Parent: Guid
        Name: string
        Master: Guid
        Template: Guid
        Templatekey: string
        Fields: List<Field>
    }     

    type ProjectTree = 
        | Root of ProjectTree seq        
        | Branch of Item * ProjectTree seq
        | Leaf of Item    

    type Parser<'t> = Parser<'t, unit>