(*************************************************************************************    
    !!!!IMPORTANT!!!!
    This is *NOT* a full implementation or even the same grammar as in the assignment. 
    
    IT IS FOR CONCEPT ILLUSTRATION PURPOSES ONLY.
    THE CODE WILL NEED TO BE MODIFIED TO ACCOMMODATE THE ASSIGNMENT.

    !!!!IMPORTANT!!!!     
**************************************************************************************)

(* 
Simple Sample Grammar: 

<program>    ->  <stmt_list> $$
<stmt_list>  ->  <stmt> <stmt_list> | ε
<stmt>       ->  id ':=' <expr> | 'write' <expr> | <for_stmt>
<term_tail>  ->  <AddOp> <term> <term_tail> | ε
<expr>       ->  <term> <term_tail>
<for_stmt>   ->  'for' id '=' id 'to' id <step_stmt> 'do' <stmt_list> 'done'
<step_stmt>  ->  'step' id | ε
<term>       ->  id 
<AddOp>      ->  '+' | '-'


Valid Samples:
x := y
write total
x := a + b - c
for i = start to finish do write i done
for i = 0 to n step 2 do x := x + i done

Invalid Samples:
x := 
write
x = y
write a +
for i = 0 to n do write i
for i = 0 to n step do x := x + i done
*)


// Tokens
type Token =
    | WRITE  // the "write" keyword
    | ASGN   // the assignment operator ":="
    | EQUAL  // the assignment operator "="
    | FOR    // the "for" keyword
    | TO     // the "to" keyword
    | DO     // the "do" keyword
    | DONE   // the "done" keyword
    | ADDOP  // addition or subtraction operators "+" or "-"
    | CONDOP // conditional operators "<", "==", etc.
    | STEP   // the "step" keyword
    | ID of string // ID may be any token/string not above/below, but ID is also bound to a string value

    // Member Function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with
        | "write" -> WRITE     // "write" maps to the WRITE token
        | ":=" -> ASGN         // ":=" maps to the ASGN token
        | "+" | "-" -> ADDOP   // "+" and "-" map to the ADD token
        | "<" | "==" -> CONDOP // "<" and "==" map to the CONDOP token
        | "=" -> EQUAL         // "=" maps to the EQUAL token
        | "step" -> STEP       // "step" maps to the STEP token
        | "for" -> FOR         // "for" maps to the FOR token
        | "to" -> TO           // "to" maps to the TO token
        | "do" -> DO           // "do" maps to the DO token
        | "done" -> DONE       // "done" maps to the DONE token
        | _ -> ID str // Everything else is just an ID



/// NOTE: The `and` keyword in F# is used to define mutually recursive functions or types.
/// It allows functions to reference other functions which have not yet been defined.
/// 
/// Classic Example:
///   ```fsharp code
///   let rec isEven x =
///       if x = 0 then true
///       else isOdd (x - 1)
///   and isOdd x =
///       if x = 0 then false
///       else isEven (x - 1)


// Start parsing by piping the list to the first rule
// TIP: to test a specific rule, just pipe the list to that rule instead of "program"
let rec start xs = 
    try
        match xs |> program with 
        | [] -> printfn "SUCCESS!!!" |> ignore 
        | _  -> printfn "Parse Failed: Unprocessed tokens remain!" |> ignore
    with Failure msg 
        -> printfn $"Parsing error... %s{msg}" |> ignore
    
    xs // Reurn the list of tokens for debugging purposes.

// <program> ::= <stmt_list> $$
and program xs =
    // Note that the "$$" is a special meta-token that represents the "Top of Stack" or "Empty List"
    // F#/OCaml allow naming functions with symbols by delimiting with double "backticks" (grave accent marks) as shown below.
    xs |> stmt_list |> ``$$``

    // NOTE: Your code should diplay a sucess or failure message within this function.


// As "$$" means "Top of Stack", this represents an "Empty List"
// Functions can be named virtually anything if in backticks.  This is a good way to name a function with a symbol.
and ``$$`` =
    function
    | [] -> [] : Token list  // Explicit Type is necessary because the type of an empty list cannot be inferred!
    | reminingElements -> failwithf $"Unprocessed Tokens: {reminingElements}"


// <stmt_list> ::= <stmt> <stmt_list> | ε
// stmt_list may be empty/epsilon, so no fail case is necessary. (if an epsilon production, the list will be returned unchanged)
and stmt_list lst =
    // Since FIRST(stmt_list) is { WRITE, FOR, ID, then it is a stmt, otherwise, it is an epsilon production.
    // "_" is a wildcard, so it will match any token. We don't care what it is, just that it is there.
    match lst with
    | ( WRITE | ID _ | FOR ) :: _ -> lst |> stmt |> stmt_list
    | x -> x // ε case, so just return the list unchanged.


// <stmt> ::= id := <expr> | read id | write <expr> | <for_stmt>
and stmt lst =
    // For debugging!
    printfn $"DEBUG stmt rule: The token list  = %A{lst}" // the is a format specifier will print the whole list

    match lst with
    | ID _ :: ASGN :: xs -> xs |> expr  // id := <expr>
    | WRITE :: xs -> xs |> expr         // write <expr>
    | FOR :: _ -> lst |> for_stmt       // <for_stmt>
 
    | _ -> failwithf $"Not a valid statement: %A{lst}" // no empty case allowed


// <expr> ::= <term> <term_tail>
and expr lst =
    // Nothing to check, so just follow and implement the rule as is.
    lst |> term |> term_tail


// <term_tail> ::=  <AddOp> <term> <term_tail> | ε
and term_tail =
    function
    | ADDOP :: xs -> xs |> term |> term_tail
    | x -> x // ε case, so just return the list unchanged.


// <term> ::= id
and term =
    function
    | ID _ :: xs -> xs
    | _ -> failwithf "Expecting an ID"



// <for_stmt> ::= 'for' id '=' id 'to' id <step_stmt> 'do' <stmt_list> 'done'
// NOTE:  This function does a lot of things in different ways ONLY TO ILLUSTRATE DIFFERENT TECHNIQUES!!!
and for_stmt lst =
    printfn $"In for_stmt: The token list  = %A{lst}" // the is a format specifier will print the whole list
    
    // A nested helper function to match the "DONE" token and return the remaining list.
    let match_DONE_token =
        function
        | DONE :: xs -> xs
        | _ -> failwith "Expected DONE"

    // A nested helper function to match the "STEP" token and return the remaining list.
    let match_STEP_token =
        function
        | STEP :: ID _ :: xs -> xs
        | xs -> xs // ε case, so just return the list unchanged.

    // All "for loops" are structured as:  "for a = b to c do ... done"
    // So, this is just an illustration of a long pattern match for a series of tokens.
    match lst with

    // The pattern match is a series of tokens that must be in order.
    // This is long pattern only to illustrate a point.  It could be broken up into smaller patterns.
    | FOR :: ID _ :: EQUAL :: ID _ :: TO :: ID _ :: xs ->
        // This is a new line only to allow for nicer indentation below.
        // If the above pattern matches, then the following will be executed.
        // The "xs" is the remaining list after the pattern match.
        xs

        // Pipe the remaining list to the "step_stmt" function, which is defined above.
        |> match_STEP_token

        // If the remaining list starts with the "DO" token, pipe the remaining list to the "stmt_list" function.
        // Of course, any number of patterns could be matched here, but this is just an example. (and it's a
        // bit simpler than using "if ... then ... else ...")
        |> function
            | DO :: xs  ->  xs |> stmt_list
            | _ -> failwith "Expected DO"

        |> match_DONE_token

    | _ -> failwith $"FOR expected but found {List.head lst}" // no empty case allowed




(* ************************************************************************************************ *
 * !!!!!!!!!!!!!!!!!!!!!!!!! DO NOT MODIFY ANY CODE BELOW THIS POINT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
 * ************************************************************************************************ *)

// Import the regex module
open System.Text.RegularExpressions

// NOTE: To make the 'let' assignment be a function that accepts 0 parameters,
// an "empty tuple" must be accepted in ML/SML/OCaml/F#. (In the ML family, there 
// is no such thing as a function which does not accept a parameter.)
let main () =
    // A simple 'helper' to help debug the mapping process.
    let displayMapping str = printfn $"Mapping \"%s{str}\" -> %O{Token.tokenFromLexeme str}"; str

    // Convert a list of stings to Tokens:
    // This is very ".NET" specific. Regex.Matches is part of the (Microsoft).NET API.        
    let getTokenList str = 
        let tokenRegex = 
            @"(?x)" +  // "@" marks a regex string, (?x) is "PCRE style" Extended mode (ignore whitespace) 
            " >= | <= | => | =< | <> | == | != | >> | << | \|= | &= | \^= | := |" +
            " \+= | -= | \*= | /= | %= | && | \|\| | \+\+ | -- | <- | -> |" + 
            " \d+ | \w+ | [^\s]"
            
        // The regex matches the following tokens:
        Regex.Matches (str, tokenRegex) |> 
        (* Seq.cast<Match> |>  // This line is only necessary when FSI.EXE is running on ".NET Framework", not on ".NET Core" *)
        Seq.toList |>
        List.map string |>
        List.map _.ToLower() |>  // Yes, a violation of not using "methods", but much simpler in this case.
        List.map (displayMapping >> Token.tokenFromLexeme)


    // Begin the Parsing Process
    let startParsing str =
        // Display our list of tokens...
        printfn $"\nInitial String: %s{str}"
   
        let tokenList = getTokenList str
        printfn $"\nToken List BEFORE Parsing: %A{tokenList}"

        let parsedList = start tokenList
        printfn $"Tokens List AFTER Parsing: %O{parsedList}"


    // Get the user input and start parsing
    let getUserInput () =
        printf "Input String\n=> "    
        System.Console.ReadLine()  // Note that console I/O uses the .NET library in F#.  
                                   // For OCaml, See: https://ocaml.org/manual/5.2/api/Stdlib.html#VALread_line

    // Get the user input and start parsing
    in getUserInput () |> startParsing |> ignore  // Just ignore the result, as we are just printing results above.


// Uncomment the following lines to run the program in continuous a loop.
// This will allow you to enter multiple strings without restarting the program.

// *** YOU MAYUNCOMMENT THIS FOR TESTING, BUTCHANGE IT BACK BEFORE SUBMITTING!!! ***
// let rec run () : unit = main (); run () 
// run ()  

// Run the program once
main ()