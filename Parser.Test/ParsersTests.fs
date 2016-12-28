module Parsers.Test
    
    open System
    open NUnit.Framework
    open FsUnit
   // open Parsers
    open FParsec

    type InitMsgUtils() =
        inherit FSharpCustomMessageFormatter()

    let testParser p str =
        match run p str with
        | Success(result, _, _)   -> result
        | Failure(errorMsg, _, _) -> failwith "unable to parse"
         
    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>

    let pguid: Parser<_> =
        let guidRegex = @"^[{(]?[0-9A-F]{8}[-]?([0-9A-F]{4}[-]?){3}[0-9A-F]{12}[)}]?$"
        regex guidRegex |>> fun a -> Guid.Parse a
                  
    [<Test>] 
    let ``parse guid`` () =
        testParser pguid "{0DE95AE4-41AB-4D01-9EB0-67441B7C2450}" |> should equal (Guid "{0DE95AE4-41AB-4D01-9EB0-67441B7C2450}")
            
