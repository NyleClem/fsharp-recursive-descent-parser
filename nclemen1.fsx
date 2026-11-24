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
| DO                 // the "do" keyword
| OD                //the "od" keyword
| READ             //the "read" keyword
| WRITE           //the "write" keyword
| IF               // the "if" keyword
| THEN            // the "then" keyword
| ELSE            // the "else" keyword
| ENDIF           //the "endif" keyword
| FOR            // the "for" keyword
| IN             // the "in" keyword
| DEF            // the "def" keyword
| WHILE          // the "while" keyword
| CALL_ASSIGN    // the "<-" operator
| EQUALS         // the "=" operator
| PARENT_START    // the "(" symbol
| PARENT_END      // the ")" symbol
| BRACKET_START   // the "[" symbol
| BRACKET_END    // the "]" symbol
| COMMA         // the "," symbol
| SUBTRACT      // the "-" operator
| ADD           // the "+" operator
| MULTIPLY      // the "*" operator
| DIVIDE        // the "/" operator
| GREATER_THAN  // the ">" operator
| LESS_THAN     // the "<" operator
| REL_EQUALS    // the "==" operator
| ID of string  // Stores the lexeme of any unrecognized token


    // Member Function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with
        | "do" -> DO
        | "od" -> OD
        | "read" -> READ
        | "write" -> WRITE
        | "if" -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | "endif" -> ENDIF
        | "for" -> FOR
        | "in" -> IN
        | "def" -> DEF
        | "while" -> WHILE
        | "<-" -> CALL_ASSIGN
        | "=" -> EQUALS
        | "(" -> PARENT_START
        | ")" -> PARENT_END
        | "[" -> BRACKET_START
        | "]" -> BRACKET_END
        | "," -> COMMA
        | "-" -> SUBTRACT
        | "+" -> ADD
        | "*" -> MULTIPLY
        | "/" -> DIVIDE
        | ">" -> GREATER_THAN
        | "<" -> LESS_THAN
        | "==" -> REL_EQUALS
        |  _ -> ID str


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
let rec start str xs =
    try
        xs |> program |> ignore
        printfn "SUCCESS!!!"
        printfn $"The input {str} follows the grammar."
    with Failure msg ->
        printfn $"The input {str} is invalid because: {msg}"
    xs
    
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
    // If the first token begins a statement, then parse the statement and continue parsing the list of statements.
    match lst with
    | (ID _ |READ|WRITE|IF|FOR|WHILE|DEF ) :: _ -> lst |> stmt |> stmt_list
    | x -> x // ε case, so just return the list unchanged.


// stmt ::= id assign | read_stmt | write_stmt | if_stmt
//        | for_stmt | while_stmt | fun_def

and stmt lst =
    match lst with
    | ID _ :: xs -> xs |> assign
    | READ :: _ -> lst |> read_stmt
    | WRITE :: _ -> lst |> write_stmt
    | IF :: _ -> lst |> if_stmt
    | FOR :: _ -> lst |> for_stmt
    | WHILE :: _ -> lst |> while_stmt
    | DEF :: _ -> lst |> fun_def
    | _ -> failwithf $"Not a valid statement: %A{lst}"
    // stmt_block ::= do stmt_list od

and stmt_block lst =
    match lst with
    | DO :: xs ->
        match xs |> stmt_list with
        | OD :: remaining -> remaining
        | _ -> failwith "Expected 'od'"
    | _ -> failwith "Expected 'do'"

    // read_stmt ::= read id
and read_stmt lst =
    match lst with
    | READ :: ID _ :: xs -> xs
    | _ -> failwith "Expected 'read' followed by an ID"


    // write_stmt ::= write expr
and write_stmt lst =
    match lst with
    | WRITE :: xs -> xs |> expr
    | _ -> failwith "Expected 'write'"

// if_stmt ::= if cond then stmt_list else_stmt

and if_stmt lst =
    match lst with
    | IF :: xs ->
        match xs |> cond with
        | THEN :: remaining -> remaining |> stmt_list |> else_stmt
        | _ -> failwith "Expected 'then'"
    | _ -> failwith "Expected 'if'"

// else_stmt ::= else stmt_list endif | endif
and else_stmt lst =
    match lst with
    | ELSE :: xs ->
        match xs |> stmt_list with
        | ENDIF :: remaining -> remaining
        | _ -> failwith "Expected 'endif'"
    | ENDIF :: xs -> xs
    | _ -> failwith "Expected 'else' or 'endif'"

    // for_stmt ::= for id in for_tail
and for_stmt lst =
    match lst with
    | FOR :: ID _ :: IN :: xs -> xs |> for_tail
    | _ -> failwith "Expected 'for id in'"

    // for_tail ::= array stmt_block | id stmt_block
and for_tail lst =
    match lst with
    | BRACKET_START :: _ -> lst |> array |> stmt_block
    | ID _ :: xs -> xs |> stmt_block
    | _ -> failwith "Expected an array or ID"

    // fun_def ::= def id ( id_list ) stmt_block
and fun_def lst =
    match lst with
    | DEF :: ID _ :: PARENT_START :: xs ->
        match xs |> id_list with
        | PARENT_END :: remaining -> remaining |> stmt_block
        | _ -> failwith "Expected ')'"
    | _ -> failwith "Expected 'def id ('"

    // while_stmt ::= while cond stmt_block
and while_stmt lst =
    match lst with
    | WHILE :: xs -> xs |> cond |> stmt_block
    | _ -> failwith "Expected 'while'"

// assign ::= <- fun_call | = assign_tail
and assign lst =
    match lst with
    | CALL_ASSIGN :: xs -> xs |> fun_call
    | EQUALS :: xs -> xs |> assign_tail
    | _ -> failwith "Expected '<-' or '='"

// assign_tail ::= expr | array
and assign_tail lst =
    match lst with
    | (ID _ | PARENT_START) :: _ -> lst |> expr
    | BRACKET_START :: _ -> lst |> array
    | _ -> failwith "Expected an expression or array"

// fun_call ::= id ( param_list )
and fun_call lst =
    match lst with
    | ID _ :: PARENT_START :: xs ->
        match xs |> param_list with
        | PARENT_END :: remaining -> remaining
        | _ -> failwith "Expected ')'"
    | _ -> failwith "Expected function name followed by '('"

    // array ::= [ id_list ]
and array lst =
    match lst with
    | BRACKET_START :: xs ->
        match xs |> id_list with
        | BRACKET_END :: remaining -> remaining
        | _ -> failwith "Expected ']'"
    | _ -> failwith "Expected '['"

// param_list ::= expr param_tail | ε
and param_list lst =
    match lst with
    | (ID _ | PARENT_START) :: _ -> lst |> expr |> param_tail
    | x -> x

    // param_tail ::= , expr param_tail | ε
and param_tail lst =
    match lst with
    | COMMA :: xs -> xs |> expr |> param_tail
    | x -> x

    // id_list ::= id id_tail
and id_list lst =
    match lst with
    | ID _ :: xs -> xs |> id_tail
    | _ -> failwith "Expected an ID"

// id_tail ::= , id_list | ε
and id_tail lst =
    match lst with
    | COMMA :: xs -> xs |> id_list
    | x -> x

// expr ::= id expr_tail | ( expr )
and expr lst =
    match lst with
    | ID _ :: xs -> xs |> expr_tail
    | PARENT_START :: xs ->
        match xs |> expr with
        | PARENT_END :: remaining -> remaining
        | _ -> failwith "Expected ')'"
    | _ -> failwith "Expected an ID or '('"

// expr_tail ::= arith_op expr | ε
and expr_tail lst =
    match lst with
    | (SUBTRACT | ADD | MULTIPLY | DIVIDE) :: _ ->
        lst |> arith_op |> expr
    | x -> x

    // cond ::= expr rel_oper expr
and cond lst =
    lst |> expr |> rel_oper |> expr

    // arith_op ::= - | + | * | /
and arith_op lst =
    match lst with
    | (SUBTRACT | ADD | MULTIPLY | DIVIDE) :: xs -> xs
    | _ -> failwith "Expected an arithmetic operator"

// rel_oper ::= > | < | ==
and rel_oper lst =
    match lst with
    | (GREATER_THAN | LESS_THAN | REL_EQUALS) :: xs -> xs
    | _ -> failwith "Expected a relational operator"


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

        let parsedList = start str tokenList
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