open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
match lookahead toks with
|Some(Tok_Let) -> parse_letexp (match_token toks Tok_Let)
|Some(Tok_If) -> parse_ifexp (match_token toks Tok_If)
|Some(Tok_Fun) -> parse_funexp (match_token toks Tok_Fun)
|_ -> parse_orexp toks

and parse_letexp toks =
let (t, b) = parserec toks in
let v = match lookahead t with Some(Tok_ID(x)) -> x in
let (t', exp) = parse_expr (match_many t [Tok_ID(v); Tok_Equal]) in
let (t'', exp2) = parse_expr (match_token t' Tok_In) in 
(t'', Let(v, b, exp, exp2))

and parserec toks = 
match lookahead toks with
|Some(Tok_Rec) -> (match_token toks Tok_Rec, true)
|_ -> (toks, false)

and parse_ifexp toks = 
let (t,exp) = parse_expr toks in
let (t', exp2) = parse_expr (match_token t Tok_Then) in
let (t'', exp3) = parse_expr (match_token t' Tok_Else) in
(t'', If(exp, exp2, exp3))

and parse_funexp toks = 
let v = match lookahead toks with Some(Tok_ID(x)) -> x in
let (t, exp) = parse_expr (match_many toks [Tok_ID(v); Tok_Arrow]) in
(t, Fun(v, exp))

and parse_orexp toks = 
let (t,exp) = parse_andexp toks in
match lookahead t with
|Some(Tok_Or) -> let (t', exp2) = parse_orexp (match_token t Tok_Or) in (t', Binop(Or, exp, exp2))
|_ -> (t, exp)

and parse_andexp toks = 
let (t,exp) = parse_eqexp toks in
match lookahead t with
|Some(Tok_And) -> let (t', exp2) = parse_andexp (match_token t Tok_And) in (t', Binop(And, exp, exp2))
|_ -> (t, exp)

and parse_eqexp toks =
let (t,exp) = parse_relexp toks in
match lookahead t with
|Some(Tok_Equal) -> let (t', exp2) = parse_eqexp (match_token t Tok_Equal) in (t', Binop(Equal, exp, exp2))
|Some(Tok_NotEqual) -> let (t', exp2) = parse_eqexp (match_token t Tok_NotEqual) in (t', Binop(NotEqual, exp, exp2))
|_-> (t,exp)

and parse_relexp toks =
let (t,exp) = parse_addexp toks in
match lookahead t with
|Some(Tok_Greater) -> let (t', exp2) = parse_relexp (match_token t Tok_Greater) in (t', Binop(Greater, exp, exp2))
|Some(Tok_GreaterEqual) -> let (t', exp2) = parse_relexp (match_token t Tok_GreaterEqual) in (t', Binop(GreaterEqual, exp, exp2))
|Some(Tok_Less) -> let (t', exp2) = parse_relexp (match_token t Tok_Less) in (t', Binop(Less, exp, exp2))
|Some(Tok_LessEqual) -> let (t', exp2) = parse_relexp (match_token t Tok_LessEqual) in (t', Binop(LessEqual, exp, exp2))
|_-> (t,exp)

and parse_addexp toks =
let (t,exp) = parse_mulexp toks in
match lookahead t with
|Some(Tok_Add) -> let (t', exp2) = parse_addexp (match_token t Tok_Add) in (t', Binop(Add, exp, exp2))
|Some(Tok_Sub) -> let (t', exp2) = parse_addexp (match_token t Tok_Sub) in (t', Binop(Sub, exp, exp2))
|_-> (t,exp)

and parse_mulexp toks =
let (t,exp) = parse_conexp toks in
match lookahead t with
|Some(Tok_Mult) -> let (t', exp2) = parse_mulexp (match_token t Tok_Mult) in (t', Binop(Mult, exp, exp2))
|Some(Tok_Div) -> let (t', exp2) = parse_mulexp (match_token t Tok_Div) in (t', Binop(Div, exp, exp2))
|_-> (t,exp)

and parse_conexp toks = 
let (t,exp) = parse_unexp toks in
match lookahead t with
|Some(Tok_Concat) -> let (t', exp2) = parse_conexp (match_token t Tok_Concat) in (t', Binop(Concat, exp, exp2))
|_ -> (t, exp)

and parse_unexp toks =
match lookahead toks with
|Some(Tok_Not) -> let (t, exp) = parse_unexp (match_token toks Tok_Not) in (t, Not(exp))
|_ -> parse_appexp toks

and parse_appexp toks = 
let (t,exp) = parse_selexp toks in
match lookahead t with 
|Some(Tok_Int(x)) -> let (t', exp2) = parse_priexp t in (t', App(exp, exp2))
|Some(Tok_Bool(x)) -> let (t', exp2) = parse_priexp t in (t', App(exp, exp2))
|Some(Tok_String(x)) -> let (t', exp2) = parse_priexp t in (t', App(exp, exp2))
|Some(Tok_ID(x)) -> let (t', exp2) = parse_priexp t in (t', App(exp, exp2))
|Some(Tok_LParen) -> let (t', exp2) = parse_priexp t in (t', App(exp, exp2))
|Some(Tok_LCurly) -> let (t', exp2) = parse_priexp t in (t', App(exp, exp2))
|_-> (t,exp)

and parse_selexp toks=
let (t,exp) = parse_priexp toks in
match lookahead t with
|Some(Tok_Dot) -> let v = match (lookahead (match_token t Tok_Dot)) with 
  |Some(Tok_ID(x)) -> x 
  |_-> raise(InvalidInputException(""))
in (match_many t [Tok_Dot;Tok_ID(v)], Select(Lab(v),exp))
|_ -> (t,exp)

and parse_priexp toks =
match lookahead toks with
|Some(Tok_Int(x)) -> (match_token toks (Tok_Int(x)), Int(x))
|Some(Tok_Bool(x)) -> (match_token toks (Tok_Bool(x)), Bool(x))
|Some(Tok_String(x)) -> (match_token toks (Tok_String(x)), String(x))
|Some(Tok_ID(x)) -> (match_token toks (Tok_ID(x)), ID(x))
|Some(Tok_LParen) -> let (t, exp) = parse_expr (match_token toks Tok_LParen) in 
(match_token t Tok_RParen, exp)
|Some(Tok_LCurly) -> parse_recexp (match_token toks Tok_LCurly)
|_->raise(InvalidInputException(""))

and parse_recexp toks = 
match lookahead toks with
|Some(Tok_RCurly) -> (match_token toks Tok_RCurly, Record([]))
|Some(Tok_ID(x)) -> let (t, exp) = parse_recbexp toks in (match_token t Tok_RCurly, Record(exp))
|_-> raise(InvalidInputException(""))

and parse_recbexp toks = 
let v = match (lookahead toks) with Some(Tok_ID(x)) -> x in
let t = match_many toks [Tok_ID(v); Tok_Equal] in
let (t', exp) = parse_expr t in
match lookahead t' with
|Some(Tok_Semi) -> let (t'', exp2) = parse_recbexp (match_token t' Tok_Semi) in (t'',(Lab(v), exp)::exp2)
|_-> (t', [(Lab(v), exp)])

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
match lookahead toks with
|Some(Tok_Def) -> parse_defmutop (match_token toks Tok_Def)
|Some(Tok_DoubleSemi) -> (match_token toks Tok_DoubleSemi, NoOp)
|_-> parse_exmutop toks

and parse_defmutop toks = 
let v = match lookahead toks with Some(Tok_ID(x)) -> x in
let (t, exp) = parse_expr (match_many toks [Tok_ID(v);Tok_Equal]) in
(match_token t Tok_DoubleSemi, Def(v, exp))

and parse_exmutop toks = 
let (t,exp) = parse_expr toks in
(match_token t Tok_DoubleSemi, Expr(exp))

