module Parsers

    open System
    open FParsec

    type Field(field: Guid, name: string, key: string) =
        member this.Field = field
        member this.Name = name
        member this.Key = string
     
    type Item = {
        id : Guid
        name : string
        fields : Map<string, Field>
    }

    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>

    let parseGuid: Parser<_> =
        let guidRegex = @"^[{(]?[0-9A-F]{8}[-]?([0-9A-F]{4}[-]?){3}[0-9A-F]{12}[)}]?$"
        regex guidRegex |>> fun a -> Guid.Parse a
    let parseKeyValue key pvalue = pstring key >>. pstring ": " >>. pvalue
    let parseKeyValueString key = parseKeyValue key (restOfLine true)
    let parseKeyValueInt key = parseKeyValue key pint16 .>> newline
    let parseKeyValueGuid key = parseKeyValue key parseGuid
    let parseField: Parser<_> = 
        pstring "----field----" >>. newline >>. 
        parseKeyValueGuid "field" .>>. 
        parseKeyValueString "name" .>>. 
        parseKeyValueString "key" |>> fun ((field, name), key) -> (key, new Field(field, name, key))
           
//let pItem = pstring "----item----" 
//    >>. newline 
//   .>>. pKeyValueInt "version" 
//   .>>. pKeyValueGuid "id" 
//   .>>. pKeyValueString "database" 
//   .>>. pKeyValueString "path" 
//   .>>. pKeyValueGuid "parent" 
//   .>>. pKeyValueString "name" 
//   .>>. pKeyValueString "master" 
//   .>>. pKeyValueString "template" 
//   .>>. pKeyValueString "templateKey" 
//   .>>. many pField |>> fun ((id, name), fields) -> { id = id; name = name; fields = Map.ofList fields}
