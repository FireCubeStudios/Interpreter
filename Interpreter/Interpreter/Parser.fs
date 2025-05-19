module Interpreter.Parser

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
    
    // todo comment
    let pwhitespaceChar = satisfy (fun c -> System.Char.IsWhiteSpace c) <?> "whitespace" // new
    let pletter         = satisfy (fun c -> System.Char.IsLetter c) <?> "letter" // new
    let palphanumeric   = satisfy (fun c -> System.Char.IsLetterOrDigit c) <?> "alphanumeric" // new

    let spaces         = many pwhitespaceChar <?> "spaces" // new, pchar '_' |>> fun x -> [x]
    let spaces1        = many1 pwhitespaceChar <?> "spaces1" // new, pchar '_' |>> fun x -> [x]

    //todo comment
    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>>. spaces >>. p2

    (*´ old
        let (.>*>.) _ _ = failwith "not implemented"
        let (.>*>) _ _  = failwith "not implemented"
        let (>*>.) _ _  = failwith "not implemented"
    *)

     // todo comments
    let parenthesise p = between (pchar '(' >*>. spaces) (spaces >*>. pchar ')') p <?> "paranthesis" // new
    let braces p = between (pchar '{' >*>. spaces) (spaces >*>. pchar '}') p <?> "brackets" // new optional


    // Convert a char list to a string
    let charListToString (lst: char list) = List.fold (fun str c -> $"{str}{c}") "" lst // new added helper function
    // todo comments 
    let parseString = pchar '"' >>. many (satisfy (fun c -> c <> '"')) .>> pchar '"' |>> charListToString <?> "string"

    // todo comments
    let pid = (pletter <|> pchar '_') .>>. many (pletter <|> pchar '_' <|> palphanumeric) |>> (fun (c, str) -> $"{c}{charListToString str}") <?> "id"
    
    // to do comment, 
    // takes an operator parser "op" and an argument parser 'a'
    // parses a string of "<op parser><whitespaces><a parser>" and returns the result of 'a'
    let unop op a  = op >*>. a
    // to do comment,
    // takes an operator parser "op" an argument parser 'a' and an argument parser 'b'
    // parses a string of "<a parser><whitespaces><op parser><whitespace><b parser>" and returns the result of 'a' and 'b' as a tuple
    let binop op a b = a .>*> op .>*>. b

    (* template *)
    let TermParse, tref = createParserForwardedToRef<aexpr>() // A2
    let ProdParse, pref = createParserForwardedToRef<aexpr>() // A3
    let AtomParse, aref = createParserForwardedToRef<aexpr>() // A4

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    // do tref := choice [AddParse; ProdParse] old

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    // do pref := choice [MulParse; AtomParse] old

    let NParse   = pint32 |>> Num <?> "Int" // n integer
    let ParParse = parenthesise TermParse // paranthesis ( Term )
   // do aref := choice [NParse; ParParse] old
    (* end of template *)

    (* new additions for aexpr & bexpr *)
    (* bexpr *)
    let BoolTermParse, btref = createParserForwardedToRef<bexpr>() // B1
    let BoolProdParse, bpref = createParserForwardedToRef<bexpr>() // B2

    let ANDParse = binop (pstring @"/\") BoolProdParse BoolTermParse |>> Conj <?> "AND"
    let ORParse = binop (pstring @"\/") BoolProdParse BoolTermParse |>> (fun (a, b) -> Not(Conj(Not(a), Not(b)))) <?> "OR"
    
    do btref := choice [ANDParse; ORParse; BoolProdParse]


    (* aexpr *)
    // A1
    let A1Parse, a1ref = createParserForwardedToRef<aexpr>() // A1, new added by me

    let CondParse = BoolTermParse .>*> (pchar '?') .>*>. (TermParse .>*> (pchar ':') .>*>. TermParse) |>> (fun (b, (x, y)) -> Cond(b, x, y)) <?> "Cond"

    do a1ref := choice [CondParse; TermParse]

    // A2
    let SubParse = binop (pchar '-') ProdParse TermParse |>> (fun (x, y) -> Add(x, Mul(Num (-1), y))) <?> "Subtraction"

    do tref := choice [AddParse; SubParse; ProdParse]

    // A3
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    // A4
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul(Num (-1), x))  <?> "Negation"
    let SquareBracketParse = between (pchar '[' >*>. spaces) (spaces >*>. pchar ']') TermParse <?> "Square brackets"
    let ReadParse = pstring "read" |>> (fun _ -> Read) <?> "Read"
    let RandomParse = pstring "random" |>> (fun _ -> Random) <?> "Random"
    let VParse = pid |>> Var <?> "Var"

    do aref := choice [ReadParse; RandomParse; NParse; NegParse; ParParse; SquareBracketParse; VParse]

    let paexpr = A1Parse <?> "aexpr"
    (* end of grammar additions *)

    (* new additions for bexpr *)

    let TrueParse = pstring "true" |>> (fun _ -> TT) <?> "True"
    let FalseParse = pstring "false" |>> (fun _ -> Not(TT)) <?> "False"
    let BoolNegationParse = unop (pchar '~') BoolProdParse |>> (fun b -> Not(b)) <?> "Boolean negation"
    let EqualityParse = binop (pchar '=') TermParse A1Parse |>> Eq <?> "Equality"
    let InequalityParse = binop (pstring "<>") TermParse A1Parse |>> (fun (a, b) -> Not(Eq(a, b))) <?> "Inequality"
    let LtParse = binop (pchar '<') TermParse A1Parse |>> Lt <?> "Lt <"
    let LtEqualParse = binop (pstring "<=") TermParse A1Parse |>> (fun (a, b) -> Conj(Lt(a, b), Eq(a, b))) <?> "Lt equality <="
    let RtParse = binop (pchar '>') TermParse A1Parse |>> (fun (a, b) -> Conj(Not(Lt(a, b)), Not(Eq(a, b)))) <?> "Rt >"
    let RtEqualParse = binop (pstring ">=") TermParse A1Parse |>> (fun (a, b) -> Not(Lt(a, b))) <?> "Rt equality >="
    let BooleanParParse = parenthesise BoolTermParse

    do bpref := choice [TrueParse; FalseParse; BoolNegationParse; EqualityParse; InequalityParse; LtParse; LtEqualParse; RtParse; RtEqualParse; BooleanParParse]

    let pbexpr = BoolTermParse <?> "bexpr"
    (* end of grammar additions *)

    (* new additions for stmnt *)
    let StmntTermParse, stref = createParserForwardedToRef<stmnt>() // S1
    let StmntProdParse, spref = createParserForwardedToRef<stmnt>() // S2

    let SequenceParse = (StmntProdParse .>> pchar ';') .>*>. StmntTermParse |>> Seq <?> "Sequence"

    do stref := choice [SequenceParse; StmntProdParse]

    let StatementVParse = pid .>*> pstring (":=") .>*>. paexpr |>> Assign <?> "Assign"
    let DeclareParse = (pstring ("declare") .>> pwhitespaceChar) >*>. pid |>> Declare <?> "Declare"
    let IfElseParse = pstring ("if") >*>. (parenthesise BoolTermParse) .>*>. (braces StmntTermParse) .>*>. (pstring("else") >*>. braces StmntTermParse) |>> (fun ((b, sIf), sElse) -> If(b, sIf, sElse)) <?> "If Else"
    let IfParse = pstring ("if") >*>. (parenthesise BoolTermParse) .>*>. (between (pchar '{' >*>. spaces) (spaces >*>. pchar '}') StmntTermParse) |>> (fun (b, s) -> If(b, s, Skip)) <?> "If"
    let WhileParse = pstring ("while") >*>. (parenthesise BoolTermParse) .>*>. (braces StmntTermParse) |>> While <?> "While"
    let AllocParse = pstring ("alloc") >>. pchar '(' >>. pid .>>. (pchar ',' >>. paexpr) .>> pchar ')' |>> Alloc <?> "Alloc"
    let FreeParse = pstring ("free") >>. pchar '(' >>. paexpr .>>. (pchar ',' >>. paexpr) .>> pchar ')' |>> Free <?> "Free"
    let PrintParse = pstring ("print") >>. pchar '(' >>. (parseString .>> pchar ',') .>*>. (many (paexpr .>> ((pchar ',' .>*> spaces) <|> (pchar ')')))) |>> (fun (s, lst) -> Print(lst, s)) <?> "Print"
    let MemParse = (pchar '[' >>. paexpr .>> pchar ']') .>*>. (pstring (":=") >*>. paexpr) |>> (fun (m1, m2) -> MemWrite(m1, MemRead(m2))) <?> "Memory"

    do spref := choice [StatementVParse; DeclareParse; IfElseParse; IfParse; WhileParse; AllocParse; FreeParse; PrintParse; MemParse]

    let pstmnt = StmntTermParse <?> "stmnt"
    (* end of grammar additions *)

    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
