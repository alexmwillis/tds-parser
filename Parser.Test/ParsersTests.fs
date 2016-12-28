module Parsers.Test
    
    open System
    open NUnit.Framework
    open FsUnit
    open Parsers
    open FParsec

    let testParser p str =
        match run p str with
        | Success(result, _, _)   -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg         
     
    [<TestFixture>]
    type ``Parse guid tests`` ()=
          
        [<TestCase("{0DE95AE4-41AB-4D01-9EB0-67441B7C2450}")>] 
        [<TestCase("0DE95AE4-41AB-4D01-9EB0-67441B7C2450")>] 
        member t.``can parse valid guid`` (guid) =
            testParser parseGuid guid |> should equal (Guid guid)
            
        [<TestCase("0de95ae4-41ab-4d01-9eb0-67441b7c2450")>] 
        member t.``can't parse invalid guid`` (guid) =
            (fun () -> testParser parseGuid guid |> ignore) |> should throw typeof<Exception>

    [<TestFixture>]
    type ``Parse key value tests`` ()=
          
        [<Test>] 
        member t.``can parse key with string value`` () =
            let strToParse = "key: hello world\n"
            testParser (parseKeyValueString "key") strToParse |> should equal "hello world"
            
        [<Test>] 
        member t.``can parse key with integer value`` () =
            let strToParse = "key: 123\n"
            testParser (parseKeyValueInt "key") strToParse |> should equal 123

        [<Test>] 
        member t.``can parse key with guid value`` () =
            let strToParse = "field: {A0CB3965-8884-4C7A-8815-B6B2E5CED162}"
            testParser (parseKeyValueGuid "field") strToParse |> should equal (Guid "A0CB3965-8884-4C7A-8815-B6B2E5CED162")

    [<TestFixture>]
    type ``Parse field tests`` ()=
          
        [<Test>] 
        member t.``can parse field`` () =
            let field = @"----field----
field: {A0CB3965-8884-4C7A-8815-B6B2E5CED162}
name: __Editors
key: __editors
content-length: 77

{59F53BBB-D1F5-4E38-8EBA-0D73109BB59B}|{A0C460F4-DBAE-4A5A-8F3A-C4ADFCDACEEA}"
            let expected = ("__editors", new Field(Guid "{A0CB3965-8884-4C7A-8815-B6B2E5CED162}", "__Editors", "__editors"))
            testParser parseField field |> should equal expected

            