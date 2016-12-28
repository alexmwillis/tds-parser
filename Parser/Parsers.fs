module Parsers

    open System
    open FParsec

    type Field = {
        value : string
    }
 
    type Item = {
        id : Guid
        name : string
        fields : Map<string, Field>
    }

    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>

    let pguid: Parser<_> =
        let guidRegex = @"^[{(]?[0-9A-F]{8}[-]?([0-9A-F]{4}[-]?){3}[0-9A-F]{12}[)}]?$"
        regex guidRegex |>> fun a -> Guid.Parse a
    let pKeyValue key pvalue = pstring key >>. pchar ':' >>. pvalue
    let pKeyValueString key = pKeyValue key (restOfLine true)
    let pKeyValueInt key = pKeyValue key pint16 .>> newline
    let pKeyValueGuid key = pKeyValue key pguid .>> newline
//let pField = pstring "-f-" >>. newline >>. pKeyValueString "key" .>>. pKeyValueString "value" |>> fun (key, value) -> (key, { value = value })
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
