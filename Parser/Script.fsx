﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I @"C:\Users\Alex\Documents\Visual Studio 2015\Projects\Parser\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsecCS.dll"
#r "FParsec.dll"

open FParsec
open System

type Field = {
    value : string
}

type Item = { 
    id : Guid
    name : string
    fields : Map<string, Field>
}

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let readLines filePath = System.IO.File.ReadAllText(filePath)

let str = @"----item----
version: 1
id: {0DE95AE4-41AB-4D01-9EB0-67441B7C2450}
database: master
path: /sitecore/content
parent: {11111111-1111-1111-1111-111111111111}
name: content
master: {00000000-0000-0000-0000-000000000000}
template: {E3E2D58C-DF95-4230-ADC9-279924CECE84}
templatekey: Main section

----field----
field: {A0CB3965-8884-4C7A-8815-B6B2E5CED162}
name: __Editors
key: __editors
content-length: 77

{59F53BBB-D1F5-4E38-8EBA-0D73109BB59B}|{A0C460F4-DBAE-4A5A-8F3A-C4ADFCDACEEA}
----field----
field: {06D5295C-ED2F-4A54-9BF2-26228D113318}
name: __Icon
key: __icon
content-length: 27

People/16x16/cubes_blue.png
----field----
field: {9C6106EA-7A5A-48E2-8CAD-F0F693B1E2D4}
name: __Read Only
key: __read only
content-length: 1

1
----field----
field: {21F74F6E-42D4-42A2-A4B4-4CEFBCFBD2BB}
name: __Facets
key: __facets
content-length: 350

{63AF4405-7F65-4079-8C62-5D09C705545B}|{154DC6DA-89C4-4704-8CDA-95994D794BEA}|{0E85C8BF-6F59-4907-9E6D-FC603573A798}|{7146F1A4-45FB-4CEC-9855-C95E9E595827}|{56EF9816-35AD-4160-B5DC-ECA7FE7DCFC2}|{5B3E125B-0F02-4EA3-9C3C-8BDC9CE34A3B}|{A9649925-64C2-4EF9-AA66-CF20D6925929}|{BC06ED64-C4A1-4EE2-9835-541E1CC4CCC9}|{8355EB8A-44EC-47FC-8FFD-6412C400A397}
----field----
field: {1172F251-DAD4-4EFB-A329-0C63500E4F1E}
name: __Masters
key: __masters
content-length: 0


----field----
field: {F2DB8BA1-E477-41F5-8EF5-22EEFA8D2F6E}
name: __Enabled Views
key: __enabled views
content-length: 116

{75DA7671-C7A8-4DDF-BB8A-9A22B3D86D7B}|{74008565-F85E-4D66-9B10-42E1A03770BC}|{68DA2D37-ABC0-4001-BF01-A3FC8D2F1BF9}
----field----
field: {DEC8D2D5-E3CF-48B6-A653-8E69E2716641}
name: __Security
key: __security
content-length: 41

au|sitecore\ServicesAPI|p*|+item:read|^*|
----version----
language: da
version: 1
revision: af4a0c32-4683-4ad5-a55e-5d0ff653a189

----field----
field: {B5E02AD9-D56F-4C41-A065-A133DB87BDEB}
name: __Display name
key: __display name
content-length: 7

Indhold
"