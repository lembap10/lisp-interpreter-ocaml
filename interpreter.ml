(* Complete Lisp Interpreter *)

(* 
  This interpreter was a project for CSCI2041 Advanced Programming Principles
  It was written collectively in class and individually in labs and as projects
  Contains 3 main parts: Scanner, Parser, and Evaluator
*)

(* Types needed for the interpreter
  thing - used in the parser and evaluator to define how to execute parts of the lisp code
  token - used to translate characters in lisp code into groups of characters
*)
type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

module Scanner =
struct
(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
   TOKENs from a file whose pathname is the string PATH. INPUT is a channel
   connected to the file. CH holds the most recently read CHAR from INPUT. *)
  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in
(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. We'd
   like to give this CHAR a name, but then we couldn't MATCH on that name. *)
  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)
  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in
(* NEXT COMMENT. Skip a comment. It starts with a ';' and ends with a newline
   '\n' or an end of file '\000'. We skip the '\n', but not the '\000'. *)
  let rec nextComment () =
    match ! ch
    with '\000' ->
           () |
         '\n' ->
           nextChar () |
         _ ->
           nextChar () ;
           nextComment ()
  in

(* NEXT END TOKEN. Read an END TOKEN. We don't skip a CHAR because there are no
   more CHARs to skip. *)
  let nextEndToken () =
    EndToken
  in
(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN that starts with PREFIX. *)
  let nextNumberToken prefix =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             NumberToken (int_of_string chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering prefix
  in
(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)
  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in
(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN that starts with PREFIX. *)
  let nextSymbolToken prefix =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling prefix
  in
(* NEXT NUMBER OR SYMBOL TOKEN. We've just read a '-', but we don't know yet if
   it starts a NUMBER TOKEN or a SYMBOL token. Skip the '-'. If we see a digit,
   then it starts a NUMBER TOKEN, otherwise it starts a SYMBOL TOKEN. *)
  let nextNumberOrSymbolToken () =
    nextChar () ;
    match ! ch
    with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "-" |
         _ ->
           nextSymbolToken "-"
  in
(* NEXT TOKEN. Look at CH to tell what TOKEN is coming next. Dispatch to the
   function that will read that TOKEN and return it. *)

  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         ';' ->
           nextComment () ;
           nextToken () |
         '-' ->
           nextNumberOrSymbolToken () |
         '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "" |
         _ ->
           nextSymbolToken ""

(* Lost? This is MAKE SCANNER's body. Initialize CH by reading the NEXT CHAR,
   and return (but do not call!) NEXT TOKEN. *)

  in nextChar () ;
     nextToken ;;

end ;;

(* Utilizing a signature *)

module type Parsers =
  sig
    val makeParser : string -> unit -> thing
    exception Can'tParse of string 
  end;;

module Parser : Parsers =
  struct
    exception Can'tParse of string ;;
    let makeParser path = 
      let nextToken = Scanner.makeScanner path in
      let token = ref (nextToken ())  in
      let rec nextThing () =  
        match !token with 
          CloseParenToken
            -> raise (Can'tParse ("Unexpected closed parentheses"))
          | EndToken 
            -> raise (Can'tParse ("Unexpected end of file"))
          | NumberToken n 
            -> token := nextToken ();
              Number n ;
          | OpenParenToken
            -> token := nextToken () ;
            nextThings ()
          | SymbolToken "nil"
            -> token := nextToken ();
              Nil 
          | SymbolToken s
            -> token := nextToken ();
             Symbol s;
              
    and nextThings () =  
      match !token with
      CloseParenToken ->
        token := nextToken () ; Nil
      | EndToken ->
        raise (Can'tParse ("Unexpected End of File"))
      | _ ->                          
        let a = nextThing () in                   
        Cons (a, nextThings ())
    in
  nextThing ;;
  end;;

(* Calling make Parser
  To get the next thing, do nextThing () *)
let nextThing = Parser.makeParser "things.txt" ;;


module type Evaluators =
sig
  val evaluate: thing -> thing
end ;;

(* EVALUATOR. The Lisp evaluator. *)

module Evaluator: Evaluators =
struct

(* OOPS. Call this with a descriptive error MESSAGE in case of error. *)

  exception EvaluatorError of string ;;

  let oops message =
    raise (EvaluatorError message) ;;

(* ENV GET. Search an environment ENV for the value of a symbol whose string is
   NAME, and return that value. If we can't find it, then call the continuation
   ETC. *)

  let envGet env name etc =
    let rec envGetting env =
      match env
      with [] ->
             etc () |
           (otherName, otherValue) :: otherEnv ->
             if name = otherName
             then otherValue
             else envGetting otherEnv
    in envGetting env ;;

(* ENV MAKE. Return a new empty environment. *)

  let envMake () =
    [] ;;

(* ENV PUT. Return a new environment like ENV, but the symbol with the string
   NAME is bound to VALUE. *)

  let envPut name value env =
    (name, value) :: env ;;

(* TEE. The Lisp symbol T. It means TRUE, because any THING that isn't NIL also
   means TRUE. *)

  let tee = Symbol "t" ;;

(* GLOBAL. The global environment. It's a variable, so DEFINE (see below) can
   change it. Initially it binds the Lisp symbols NIL and T. *)

  let global = ref (envMake ()) ;;

  global := envPut "nil" Nil (! global) ;;
  global := envPut "t"   tee (! global) ;;

(* LOOKUP. Return the value of a symbol whose string is NAME. First search the
   local environment ENV. If we can't find NAME there, then search GLOBAL, the
   global environment. It's an error if we can't find NAME there either. *)

  let lookup env name =
    envGet env name
      (fun () ->
         envGet (! global) name
           (fun () ->
             oops ("Unbound name " ^ name))) ;;

(* EVALUATING. Do all the work for EVALUATE. *)

  let rec evaluating thing env =
    match thing
    with Cons (func, args) ->
           (match (evaluating func env)
            with Closure(pars, body, bodyEnv) ->
                   apply pars args env body bodyEnv |
                 Primitive howTo ->
                   howTo args env |
                 _ ->
                   oops "Closure or Primitive expected") |
         Symbol name ->
           lookup env name |
         _ ->
           thing

(* APPLY. Apply a CLOSURE whose parameter list is PARS, whose argument list is
   ARGS, and whose body is BODY. Arguments in ARGS are evaluated in ARGS ENV,
   and BODY is evaluated in BODY ENV, after bindings for PARS are added to 
   it. *)

  and apply pars args argsEnv body bodyEnv =
    let rec applying pars args bodyEnv =
      match (pars, args)
      with (Nil, Nil) ->
              evaluating body bodyEnv |
           (Nil, Cons (_, _)) ->
              oops "More arguments than parameters" |
           (Cons (_, _), Nil) ->
              oops "Fewer arguments than parameters" |
           (Cons (Symbol name, pars), Cons (arg, args)) ->
              applying pars args
                (envPut name (evaluating arg argsEnv) bodyEnv) |
           _ ->
              oops "Bad application"
    in applying pars args bodyEnv ;;

(* EVALUATE. Evaluate THING in the GLOBAL environment. The local environment
   (returned by ENV MAKE) is initially empty. *)

  let evaluate thing =
    evaluating thing (envMake ()) ;;

(* IS MEMBER. Test if THING is a member of the Lisp list THINGS. It's a helper
   for LAMBDA (see below). *)

  let rec isMember thing things =
    match things
    with Cons (first, rest) ->
           thing = first || isMember thing rest |
         _ ->
           false ;;

(* ARE PARAMETERS. Test if THINGS is a Lisp list of SYMBOLs, in which no SYMBOL
   appears more than once. It's another helper for LAMBDA (see below). *)

  let rec areParameters things =
    match things
    with Nil ->
           true |
         Cons (first, rest) ->
           (match first
            with Symbol _ ->
                   not (isMember first rest) && areParameters rest |
                 _ ->
                   false) |
         _ -> false ;;

(* MAKE ARITHMETIC. Return a HOW TO function that takes two NUMBER arguments,
   evaluates them both, and computes a new NUMBER from them using the OCaml
   function OP. If that doesn't work then assert an error MESSAGE. *)

  let makeArithmetic op message =
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (let left = evaluating left env
              in let right = evaluating right env
                 in match (left, right)
                    with (Number left, Number right) ->
                           Number (op left right) |
                         _ ->
                           oops message) |
           _ ->
             oops message) ;;

(* MAKE RELATION. Return a HOW TO function that takes two NUMBER arguments,
   evaluates them both, compares them using the OCaml function OP, and returns
   either NIL or T. If that doesn't work then assert an error MESSAGE. *)

  let makeRelation op message =
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (let left = evaluating left env
              in let right = evaluating right env
                 in match (left, right)
                    with (Number left, Number right) ->
                           if op left right
                           then tee
                           else Nil |
                         _ ->
                           oops message) |
           _ ->
             oops message) ;;

(* PRIMITIVE. Bind a symbol with the string NAME to a PRIMITIVE that contains
   the OCaml function HOW TO. *)

  let primitive name howTo =
    global := envPut name (Primitive howTo) (! global) ;;

(* *. Multiply two NUMBER arguments and return a NUMBER. We must write the name
   of the OCaml multiplication function with extra blanks so it won't be read
   as a comment. *)

  primitive "*" (makeArithmetic ( * ) "* expected 2 NUMBERs") ;;

(* +. Add two NUMBER arguments and return a NUMBER. *)

  primitive "+" (makeArithmetic (+) "+ expected 2 NUMBERs") ;;

(* -. Negate a single NUMBER argument, or subtract two NUMBER arguments. Return
   a NUMBER. *)

  primitive "-"
    (fun args env ->
      match args
      with Cons (right, Nil) ->
             (match (evaluating right env)
              with Number right ->
                     Number (- right) |
                   _ ->
                     oops "- expected 1 or 2 NUMBERs") |
           Cons (left, Cons (right, Nil)) ->
             let left = evaluating left env
             in let right = evaluating right env
                in (match (left, right)
                    with (Number left, Number right) ->
                           Number (left - right) |
                         _ ->
                           oops "- expected 1 or 2 NUMBERs") |
           _ ->
             oops "- expected 1 or 2 NUMBERs") ;;

(* /. Divide two NUMBER arguments and return a NUMBER. If the second argument
   is 0 then assert an error instead. *)

  primitive "/"
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             let left = evaluating left env
             in let right = evaluating right env
                in (match (left, right)
                    with (Number 0, _) ->
                           oops "/ tried to divide by 0" |
                         (Number left, Number right) ->
                           Number (left / right) |
                         _ ->
                           oops "/ expected 2 NUMBERs") |
           _ ->
             oops "/ expected 2 NUMBERs") ;;

(* <, <=, <>, >, >=. Comparisons that take two NUMBERs and return T or NIL. *)

  primitive "<"  (makeRelation (<)   "< expected 2 NUMBERs") ;;
  primitive "<=" (makeRelation (<=) "<= expected 2 NUMBERs") ;;
  primitive "<>" (makeRelation (<>) "<> expected 2 NUMBERs") ;;
  primitive ">"  (makeRelation (>)   "> expected 2 NUMBERs") ;;
  primitive ">=" (makeRelation (>=) ">= expected 2 NUMBERs") ;;

(* =. Test if two ATOMs are equal and return T or NIL. *)

  primitive "="
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             let left = evaluating left env
             in let right = evaluating right env
                in (match (left, right)
                    with (Nil, Nil) ->
                           tee |
                         (Number _, Number _) | (Symbol _, Symbol _) ->
                           if left = right
                           then tee
                           else Nil |
                         (_, _) ->
                           oops "= expected 2 ATOMs") |
           _ ->
             oops "= expected 2 ATOMs") ;;

(* AND. If there are no arguments then return T. Otherwise evaluate arguments
   from left to right. If one returns NIL, then return NIL without evaluating
   the remaining arguments. Otherwise return the result of evaluating the last
   argument. *)

  primitive "and"
    (fun args env ->
      let rec anding args =
        match args
        with Nil ->
               tee |
             Cons (arg, Nil) ->
               evaluating arg env |
             Cons (arg, args) ->
               if (evaluating arg env) = Nil
               then Nil
               else anding args |
             _ ->
               oops "AND expected 0 or more THINGs"
      in anding args) ;;

(* ATOM. Test if the single argument is a NUMBER or a SYMBOL, returning either
   T or NIL. *)

  primitive "atom"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Number _ | Symbol _ ->
                     tee |
                   _ ->
                     Nil) |
           _ ->
             oops "ATOM expected 1 THING") ;;

(* CAR. Return the first element of a Lisp list. *)

  primitive "car"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Cons (first, _) ->
                     first |
                   _ ->
                     oops "CAR expected a CONS") |
           _ ->
             oops "CAR expected a CONS") ;;

(* CDR. Return a list without its first element. *)

  primitive "cdr"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Cons (_, rest) ->
                     rest |
                   _ ->
                     oops "CDR expected a CONS") |
           _ ->
             oops "CDR expected a CONS") ;;

(* CONS. Return a new list, whose first element is the first argument and whose
   remaining elements are in the second argument. *)

  primitive "cons"
    (fun args env ->
      match args
      with Cons (first, Cons (rest, Nil)) ->
             let first = evaluating first env
             in let rest = evaluating rest env
                in (match rest
                    with Cons (_, _) ->
                           Cons (first, rest) |
                         _ ->
                           oops "CONS expected a THING and a CONS") |
           _ ->
             oops "CONS expected a THING and a CONS") ;;

(* DEFINE. Take 2 arguments: a SYMBOL and a THING. Bind the unevaluated SYMBOL
   to the result of evaluating the THING, in the GLOBAL environment. Return the
   SYMBOL. *)

  primitive "define"
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (match left
              with Symbol name ->
                     global := envPut name (evaluating right env) (! global) ;
                     left |
                   _ ->
                     oops "DEFINE expected a SYMBOL and a THING") |
           _ ->
             oops "DEFINE expected a SYMBOL and a THING") ;;

(* IF. Take 3 arguments: TEST, WHEN TRUE, and WHEN FALSE, all of which may be
   THINGs. First evaluate TEST. If it returns NIL then return the result of
   evaluating WHEN FALSE. If it returns any other THING then return the result
   of evaluating WHEN TRUE instead. *)

  primitive "if"
    (fun args env ->
      match args
      with Cons (test, Cons (whenTrue, Cons (whenFalse, Nil))) ->
             if (evaluating test env) = Nil
             then evaluating whenFalse env
             else evaluating whenTrue env |
           _ ->
             oops "IF expected 3 THINGs") ;;

(* OR. If there are no arguments then return NIL. Otherwise evaluate arguments
   from left to right. If one returns a THING that is not NIL, then return its
   result without evaluating the remaining arguments. Otherwise return the
   result of evaluating the last argument. *)

  primitive "or"
    (fun args env ->
      let rec oring args =
        match args
        with Nil ->
               Nil |
             Cons (arg, Nil) ->
               evaluating arg env |
             Cons (arg, args) ->
               let value = evaluating arg env
               in if value = Nil
                  then oring args
                  else value |
             _ ->
               oops "OR expected 0 or more THINGs"
      in oring args) ;;

(* LAMBDA, λ. Return a closure. Its parameter list is the unevaluated first
   argument, which must be a Lisp list of discrete SYMBOLs. Its body is the
   unevaluated second argument. If the current environment is GLOBAL, then the
   closure's environment is the empty environment. Otherwise the environment
   is the current one, in ENV. *)

  let howToLambda args env =
    match args
    with Cons (pars, Cons (body, Nil)) ->
           if areParameters pars
           then Closure (pars, body,
                  (if env == (! global)
                   then envMake ()
                   else env))
           else oops "LAMBDA or λ expected parameters and a body" |
         _ ->
           oops "LAMBDA or λ expected parameters and a body" ;;

  primitive "lambda" howToLambda ;;
  primitive "λ"      howToLambda ;;

(* LIST. Return a Lisp list whose elements are the result of evaluating all the
   arguments. If there are no arguments then return NIL. *)

  primitive "list"
    (fun args env ->
      let rec listing args =
        match args
        with Nil ->
               Nil |
             Cons (arg, args) ->
               Cons (evaluating arg env, listing args) |
             _ ->
               oops "LIST expected 0 or more THINGs"
      in listing args) ;;

(* NOT. If the single argument is NIL, then return T, otherwise return NIL. *)

  primitive "not"
    (fun args env ->
      match args
      with Cons(arg, Nil) ->
             if (evaluating arg env) = Nil
             then tee
             else Nil |
           _ ->
             oops "NOT expected 1 THING") ;;

(* QUOTE. Return the single argument without evaluating it. *)

  primitive "quote"
    (fun args _ ->
      match args
      with Cons (thing, Nil) ->
             thing |
           _ ->
             oops "QUOTE expected 1 THING") ;;

exception EvaluateError;;

(* Number returns the value of a number token*)
primitive "number" 
        (fun args env -> 
        match args with
        Cons (thing, Nil) ->
          (let left = evaluating thing env 
          in match left with
            Number a -> tee |
            _ -> Nil)|
        _ -> raise EvaluateError );;

(* Imply evaluates a list of things *)
primitive "imply" 
        (fun args env -> 
      let rec implying args env = 
        match args with 
        Cons(arg, Nil) -> evaluating arg env|
        Cons(arg, args) -> 
        (let value = evaluating arg env
        in if (value) = Nil
           then tee
           else implying args env )|
        _ -> raise EvaluateError
        in implying args env);;

(* Assigns a string to a value in the environment *)
primitive "let"
        (fun args env ->
          match args with
          Cons (left, Cons (middle, Cons (right, Nil))) -> 
            (match left with 
             Symbol a -> (evaluating right (envPut a middle env))|
             _ -> raise EvaluateError)|
          _ -> raise EvaluateError);;
          
end ;;

let nextThing = Parser.makeParser "things.txt" ;;
Evaluator.evaluate (nextThing ()) ;;
Evaluator.evaluate (nextThing ()) ;;
