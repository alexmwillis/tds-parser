module File1

open System

type SitecoreField =
    | Value of Map<string, string>

type SitecoreItem =
    | Id of Guid
    | Name of string
    | Fields of Map<string, SitecoreField>

/// Type that represents Success/Failure in parsing
type Result<'a> =
    | Success of 'a
    | Failure of string 
     
type ItemParserType = string -> Result<SitecoreItem>

let itemParser:ItemParserType =
    SitecoreItem (Guid.NewGuid, "myItem") 

/// Type that wraps a parsing function
type Parser<'T> = Parser of (string -> Result<'T * string>)

let parseChar charToMatch =
    Parser (fun str -> 
        match str with
        | "" -> Failure "No more input"
        | x when x.[0] = charToMatch -> Success (charToMatch, x.[1..])
        | x -> Failure (sprintf "Expecting '%c'. Got '%c'" charToMatch x.[0]))

///// Run a parser with some input
//let run parser input = 
//    // unwrap parser to get inner function
//    let (Parser innerFn) = parser 
//    // call inner function with input
//    innerFn input

/// Combine two parsers as "A andThen B"
let andThen parser1 parser2 =
    let innerFn input =
        // run parser1 with the input
        let result1 = parser1 input
        
        // test the result for Failure/Success
        match result1 with
        | Failure err -> Failure err  
        | Success (value1,remaining1) -> 
            // run parser2 with the remaining input
            let result2 =  parser2 remaining1
            
            // test the result for Failure/Success
            match result2 with 
            | Failure err ->
                // return error from parser2 
                Failure err 
            
            | Success (value2,remaining2) -> 
                // combine both values as a pair
                let newValue = (value1,value2)
                // return remaining input after parser2
                Success (newValue,remaining2)

    // return the inner function
    Parser innerFn 

/// Infix version of andThen
let ( .>>. ) = andThen

/// Combine two parsers as "A orElse B"
let orElse parser1 parser2 =
    let innerFn input =
        // run parser1 with the input
        let result1 = run parser1 input

        // test the result for Failure/Success
        match result1 with
        | Success result -> 
            // if success, return the original result
            result1

        | Failure err -> 
            // if failed, run parser2 with the input
            let result2 = run parser2 input

            // return parser2's result
            result2 

    // return the inner function
    Parser innerFn 

/// Infix version of orElse
let ( <|> ) = orElse

/// Choose any of a list of parsers
let choice listOfParsers = 
    List.reduce ( <|> ) listOfParsers 

/// Choose any of a list of characters
let anyOf listOfChars = 
    listOfChars
    |> List.map pchar // convert into parsers
    |> choice