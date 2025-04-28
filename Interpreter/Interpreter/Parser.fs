module Interpreter.JParsec

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open Parser.JParsec.TextParser             // Example parser combinator library.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use if performance gets bad
    
    // to do comments:
    let pif       : Parser<string> = pstring "if"
    let pelse     : Parser<string> = pstring "else"
    let palloc    : Parser<string> = pstring "alloc"
    let pfree     : Parser<string> = pstring "free"
    let pwhile    : Parser<string> = pstring "while"
    let pdo       : Parser<string> = pstring "do"
    let pdeclare  : Parser<string> = pstring "declare"
    let ptrue     : Parser<string> = pstring "true"
    let pfalse    : Parser<string> = pstring "false"
    let pprint    : Parser<string> = pstring "print"
    let prandom   : Parser<string> = pstring "random"
    let pread     : Parser<string> = pstring "read"
    let pfunction : Parser<string> = pstring "function"
    let pret      : Parser<string> = pstring "ret"
    
    let pwhitespaceChar = satisfy (fun c -> System.Char.IsWhiteSpace c) // new
    let pletter         = satisfy (fun c -> System.Char.IsLetter c) // new
    let palphanumeric   = satisfy (fun c -> System.Char.IsLetterOrDigit c) // new

    let spaces         = pchar '_' |>> fun x -> [x]
    let spaces1        = pchar '_' |>> fun x -> [x]

    let (.>*>.) _ _ = failwith "not implemented"
    let (.>*>) _ _  = failwith "not implemented"
    let (>*>.) _ _  = failwith "not implemented"

    let parenthesise p = p // incorrect (not implemented)
    let parseString = pstring "not implemented"

    let pid = pstring "not implemented"

    
    let unop _ = failwith "not implemented"
    let binop _ = failwith "not implemented"

    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> Num <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let paexpr = pstring "not implemented" |>> (fun _ -> Num 42)

    let pbexpr = pstring "not implemented" |>> (fun _ -> TT)

    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
