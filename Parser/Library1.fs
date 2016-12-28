module Parser

open System

type SitecoreField =
    | Values of Map<string, string>

type SitecoreItem =
    | Id of Guid
    | Name of string
    | Fields of Map<string, SitecoreField>

let parseField str =
    match str with
        
        | _ -> 

let parseFields str =
    str

let parseItem str =
    match str with
        | null | "" -> failwith "unable to parse item"
        | _ -> parseFields parseHeader _    