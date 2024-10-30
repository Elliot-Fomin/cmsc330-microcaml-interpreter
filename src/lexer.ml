open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec tokenize input = 
let len = String.length input in
let lp = Re.compile (Re.Perl.re "^\\(" ) in
let rp = Re.compile (Re.Perl.re "^\\)") in
let lc = Re.compile (Re.Perl.re "^\\{") in
let rc = Re.compile (Re.Perl.re "^\\}") in
let dot = Re.compile (Re.Perl.re "^\\.") in
let equ = Re.compile (Re.Perl.re "^\\=") in
let ne = Re.compile (Re.Perl.re "^\\<\\>") in
let gr = Re.compile (Re.Perl.re "^\\>") in
let le = Re.compile (Re.Perl.re "^\\<") in
let gre = Re.compile (Re.Perl.re "^\\>\\=") in
let lee = Re.compile (Re.Perl.re "^\\<\\=") in
let o = Re.compile (Re.Perl.re "^\\|\\|") in
let a = Re.compile (Re.Perl.re "^\\&\\&") in
let ad = Re.compile (Re.Perl.re "^\\+") in
let su = Re.compile (Re.Perl.re "^\\-") in
let mu = Re.compile (Re.Perl.re "^\\*") in
let di = Re.compile (Re.Perl.re "^\\/") in
let cc = Re.compile (Re.Perl.re "^\\^") in
let ar = Re.compile (Re.Perl.re "^\\-\\>") in
let ds = Re.compile (Re.Perl.re "^\\;\\;") in
let se = Re.compile (Re.Perl.re "^\\;") in
let pi = Re.compile (Re.Perl.re "^([0-9]+)") in
let ni = Re.compile (Re.Perl.re "^\\(-[0-9]+\\)") in 
let str = Re.compile (Re.Perl.re "^(\"[^\"]*\")" ) in
let idre = Re.compile (Re.Perl.re "^([a-zA-Z][a-zA-Z0-9]*)") in
let ws = Re.compile (Re.Perl.re "(\\s+)") in 

if input = "" then []
else if Re.execp idre input then
    let idgroup = Re.exec idre input in
    let id = Re.Group.get idgroup 0 in
    let idlen = String.length id in
    if id = "not" then
        Tok_Not::(tokenize (String.sub input 3 (len - 3)))
    else if id = "if" then
        Tok_If::(tokenize (String.sub input 2 (len - 2)))
    else if id = "else" then
        Tok_Else::(tokenize (String.sub input 4 (len - 4)))
    else if id = "then" then
        Tok_Then::(tokenize (String.sub input 4 (len - 4)))
    else if id = "let" then
        Tok_Let::(tokenize (String.sub input 3 (len - 3)))
    else if id = "def" then
        Tok_Def::(tokenize (String.sub input 3 (len - 3)))
    else if id = "in" then
        Tok_In::(tokenize (String.sub input 2 (len - 2)))
    else if id = "rec" then
        Tok_Rec::(tokenize (String.sub input 3 (len - 3)))
    else if id = "fun" then
        Tok_Fun::(tokenize (String.sub input 3 (len - 3)))
    else if id = "true" then
        Tok_Bool(true)::(tokenize (String.sub input 4 (len - 4)))
    else if id = "false" then
        Tok_Bool(false)::(tokenize (String.sub input 5 (len - 5)))
    else Tok_ID(id)::(tokenize (String.sub input idlen (len-idlen)))
else if Re.execp pi input then
    let numgroup = Re.exec pi input in
    let num = Re.Group.get numgroup 0 in
    let numlen = String.length num in
    let numint = int_of_string num in
    Tok_Int(numint)::(tokenize (String.sub input numlen (len - numlen)))
else if Re.execp ni input then
    let numgroup = Re.exec ni input in
    let strlen = (String.length (Re.Group.get numgroup 0)) in
    let num = String.sub (Re.Group.get numgroup 0) 1 (strlen-2) in
    let numint = int_of_string num in
    Tok_Int(numint)::(tokenize (String.sub input strlen (len - strlen)))
else if Re.execp ar input then
    Tok_Arrow::(tokenize (String.sub input 2 (len - 2)))
else if Re.execp lp input then
    Tok_LParen::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp rp input then
    Tok_RParen::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp lc input then
    Tok_LCurly::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp rc input then
    Tok_RCurly::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp dot input then
    Tok_Dot::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp equ input then
    Tok_Equal::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp ne input then
    Tok_NotEqual::(tokenize (String.sub input 2 (len - 2)))
else if Re.execp gr input then
    Tok_Greater::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp le input then
    Tok_Less::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp gre input then
    Tok_GreaterEqual::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp lee input then
    Tok_LessEqual::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp o input then
    Tok_Or::(tokenize (String.sub input 2 (len - 2)))
else if Re.execp a input then
    Tok_And::(tokenize (String.sub input 2 (len - 2)))
else if Re.execp ad input then
    Tok_Add::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp su input then
    Tok_Sub::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp mu input then
    Tok_Mult::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp di input then
    Tok_Div::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp cc input then
    Tok_Concat::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp ds input then
    Tok_DoubleSemi::(tokenize (String.sub input 2 (len - 2)))
else if Re.execp se input then
    Tok_Semi::(tokenize (String.sub input 1 (len - 1)))
else if Re.execp str input then
    let strgroup = Re.exec str input in
    let strlen = (String.length (Re.Group.get strgroup 0)) in
    let s = String.sub (Re.Group.get strgroup 0) 1 (strlen-2) in
    Tok_String(s)::(tokenize (String.sub input strlen (len-strlen)))
else if Re.execp ws input then
    let wsgroup = Re.exec ws input in 
    let ws = Re.Group.get wsgroup 0 in 
    let wslen = String.length ws in 
    (tokenize (String.sub input wslen (len - wslen)))
else
    failwith "lexing error"








