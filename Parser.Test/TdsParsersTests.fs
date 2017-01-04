module Parsers.Test
    
    open System
    open System.IO
    open NUnit.Framework
    open FsUnit    
    open FParsec
    open TdsParser.Types
    open TdsParser.Parsers
     
    [<TestFixture>]
    type ``Parse guid tests`` ()=
          
        [<TestCase("{0DE95AE4-41AB-4D01-9EB0-67441B7C2450}")>] 
        [<TestCase("0DE95AE4-41AB-4D01-9EB0-67441B7C2450")>] 
        [<TestCase("0de95ae4-41ab-4d01-9eb0-67441b7c2450")>] 
        member t.``can parse valid guid`` (guid) =
            runParser parseGuid guid |> should equal (Guid guid)
            
        [<TestCase("0de95ae4-41ab-4d01-9eb0-67441b7Z2450")>] 
        member t.``can't parse invalid guid`` (guid) =
            (fun () -> runParser parseGuid guid |> ignore) |> should throw typeof<Exception>

    [<TestFixture>]
    type ``Parse key value tests`` ()=
          
        [<Test>] 
        member t.``can parse key with string value`` () =
            let stringToParse = "key: hello world\n"
            runParser (parseKeyValueString "key") stringToParse |> should equal "hello world"
            
        [<Test>] 
        member t.``can parse key with integer value`` () =
            let stringToParse = "key: 123\n"
            runParser (parseKeyValueInt "key") stringToParse |> should equal 123

        [<Test>] 
        member t.``can parse key with guid value`` () =
            let stringToParse = "field: {A0CB3965-8884-4C7A-8815-B6B2E5CED162}\r\n"
            runParser (parseKeyValueGuid "field") stringToParse |> should equal (Guid "A0CB3965-8884-4C7A-8815-B6B2E5CED162")

    [<TestFixture>]
    type ``Parse content tests`` ()=

        [<Test>]
        member t.``can parse content`` ()=
            let stringToParse = @"content-length: 12

test-content
"
            let actualContent = runParser parseFieldContent stringToParse
            actualContent |> should equal "test-content"

    [<TestFixture>]
    type ``Parse field tests`` ()=
          
        [<Test>] 
        member t.``can parse field`` () =
            let stringToParse = @"----field----
field: {A0CB3965-8884-4C7A-8815-B6B2E5CED162}
name: __Editors
key: __editors
content-length: 12

test-content
"
            let expectedField =
                { 
                    FieldId = Guid "{A0CB3965-8884-4C7A-8815-B6B2E5CED162}";
                    Name = "__Editors";
                    Key = "__editors";
                    Value = "test-content"
                    Version = None
                }

            let actualField = runParser parseSharedField stringToParse 

            actualField |> should equal expectedField

        [<Test>] 
        member t.``can parse field with no content`` () =
            let stringToParse = @"----field----
field: {1172F251-DAD4-4EFB-A329-0C63500E4F1E}
name: __Masters
key: __masters
content-length: 0


"            
            let actualField = runParser parseSharedField stringToParse 

            actualField.Value |> should equal ""
            
    [<TestFixture>]
    type ``Parse versioned fields tests`` ()=

        [<Test>]
        member t.``can parse version`` () =
            let stringToParse = "----version----
language: en
version: 1
revision: 482b69be-2cd3-4d45-809e-015d7bf5ec5c

"
            let expectedVersion = { 
                Language = Language "en"; 
                Version = 1; 
                Revision = Guid "482b69be-2cd3-4d45-809e-015d7bf5ec5c"
            } 
            
            let actualVersion = runParser parseVersion stringToParse 

            actualVersion |> should equal expectedVersion
                      
        [<Test>] 
        member t.``can parse versioned fields`` () =
            let stringToParse = "----version----
language: en
version: 1
revision: 482b69be-2cd3-4d45-809e-015d7bf5ec5c

----field----
field: {B5E02AD9-D56F-4C41-A065-A133DB87BDEB}
name: __Display name
key: __display name
content-length: 7

Content
----field----
field: {577F1689-7DE4-4AD2-A15F-7FDC1759285F}
name: __Long description
key: __long description
content-length: 43

This section contains all web site content.
"
            let expectedVersion = Some { Language = Language "en"; Version = 1; Revision = Guid "482b69be-2cd3-4d45-809e-015d7bf5ec5c"} 
            let expectedFields = [|
                { 
                    FieldId = Guid "{B5E02AD9-D56F-4C41-A065-A133DB87BDEB}";
                    Name = "__Display name";
                    Key = "__display name";
                    Value = "Content"
                    Version = expectedVersion
                };
                { 
                    FieldId = Guid "{577F1689-7DE4-4AD2-A15F-7FDC1759285F}";
                    Name = "__Long description";
                    Key = "__long description";
                    Value = "This section contains all web site content."
                    Version = expectedVersion
                }|]
            let actualFields = runParser parseVersionedFields stringToParse 

            actualFields |> should equal expectedFields

        [<TestCase("test-item.item")>] 
        member t.``can parse item`` (filePath) =

            let stringToParse = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, filePath))

            let expectedVersion = Some { 
                Language = Language "en"
                Version = 1
                Revision = Guid "48a821bc-7001-4e1d-b50f-eb5f7c3414c5"
            } 
            let expectedItem = { 
                Version = 1;
                Id = Guid "{DFF1470E-984C-4B28-8DD2-4B2DCFB428BB}"; 
                Database = "master";
                Path = "/test/path";
                Parent = Guid "{0F40C120-6716-4754-9751-D7483A1E6E5F}";
                Name = "Item";
                Master = Guid "{00000000-0000-0000-0000-000000000000}";
                Template = Guid "{46C5A3B1-DA6F-4B14-94F3-3CD1D278E615}";
                Templatekey = "TemplateKey";
                Fields = 
                    [{
                        FieldId = Guid "{A57BACA0-FD8C-43A4-80BE-A034DBD6233A}"
                        Name = "HostName"
                        Key = "hostname"
                        Value = "test.com"
                        Version = None
                    };
                    {
                        FieldId = Guid "{81D4A193-0127-43CF-9D14-24C4D1E82E52}"
                        Name = "VirtualFolder"
                        Key = "virtualfolder"
                        Value = "/mc"
                        Version = None
                    };
                    {
                        FieldId = Guid "{25BED78C-4957-4165-998A-CA1B52F67497}"
                        Name = "__Created"
                        Key = "__created"
                        Value = "20161207T145810Z"
                        Version = expectedVersion
                    };
                    {
                        FieldId = Guid "{BADD9CF9-53E0-4D0C-BCC0-2D784C282F6A}"
                        Name = "__Updated by"
                        Key = "__updated by"
                        Value = "sitecore\\admin"
                        Version = expectedVersion
                    }
                ]
            }
            let actualItem = runParser parseItem stringToParse 

            actualItem |> should equal expectedItem
