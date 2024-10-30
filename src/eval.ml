open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = match e with
|Int(x) -> e
|Bool(x)-> e
|String(x) -> e
|ID(x) -> lookup env x
|Not(x) -> (match (eval_expr env x) with 
  |Bool(x) -> Bool(not x)
  |_-> raise(TypeError("Expected type bool")))
|Binop(x,y,z) -> (let a = (eval_expr env y) in let b = (eval_expr env z) in match x with
  |Add -> (match a,b with 
    |Int(n),Int(m) -> Int(n+m)
    |_ -> raise(TypeError("Expected type int")))
  |Sub -> (match a,b with 
    |Int(n),Int(m) -> Int(n-m)
    |_ -> raise(TypeError("Expected type int")))
  |Mult -> (match a,b with 
    |Int(n),Int(m) -> Int(n*m)
    |_ -> raise(TypeError("Expected type int")))
  |Div -> (match a,b with 
    |Int(n),Int(m) -> if m = 0 then raise(DivByZeroError) else Int(n/m)
    |_ -> raise(TypeError("Expected type int")))
  |Greater -> (match a,b with 
    |Int(n),Int(m) -> Bool(n>m)
    |_ -> raise(TypeError("Expected type int")))
  |Less -> (match a,b with 
    |Int(n),Int(m) -> Bool(n<m)
    |_ -> raise(TypeError("Expected type int")))
  |GreaterEqual -> (match a,b with 
    |Int(n),Int(m) -> Bool(n>=m)
    |_ -> raise(TypeError("Expected type int")))
  |LessEqual -> (match a,b with 
    |Int(n),Int(m) -> Bool(n<=m)
    |_ -> raise(TypeError("Expected type int")))
  |Concat -> (match a,b with 
    |String(n),String(m) -> String(n^m)
    |_ -> raise(TypeError("Expected type string")))
  |Equal -> (match a,b with 
    |Int(n),Int(m) -> Bool(n=m)
    |String(n),String(m) -> Bool(n=m)
    |Bool(n),Bool(m) -> Bool(n=m)
    |_ -> raise(TypeError("Expected same type")))
  |NotEqual -> (match a,b with 
    |Int(n),Int(m) -> Bool(n<>m)
    |String(n),String(m) -> Bool(n<>m)
    |Bool(n),Bool(m) -> Bool(n<>m)
    |_ -> raise(TypeError("Expected same type")))
  |Or -> (match a,b with 
    |Bool(n),Bool(m) -> Bool(n||m)
    |_ -> raise(TypeError("Expected type bool")))
  |And -> (match a,b with 
    |Bool(n),Bool(m) -> Bool(n&&m)
    |_ -> raise(TypeError("Expected type bool"))))
|If(x,y,z) -> (let a = (eval_expr env x) in match a with
  |Bool(x)-> if x then (eval_expr env y) else (eval_expr env z)
  |_-> raise(TypeError("Bool expected")))  
|Let(a,false,c,d) -> let x = eval_expr env c in let newenv = extend env a x in eval_expr newenv d
|Let(a,true,c,d) -> let newenv = (extend_tmp env a) in 
let aaa = update newenv a (eval_expr newenv c) in eval_expr newenv d
|Fun(v,x) -> Closure(env, v, x)
|App(exp1,exp2) -> (match eval_expr env exp1 with 
  |Closure(a,x,e) -> let newa = extend a x (eval_expr env exp2) in eval_expr newa e 
  |_ -> raise(TypeError("not a function")))
|Record(x)-> e
|Select(x,y) -> (match eval_expr env y with
  |Record(z)-> let rec helper lst lab = match lst with
    |[]-> raise(SelectError("Label not found"))
    |x::xs -> let (a,b) = x in if a = lab then b else helper xs lab
  in eval_expr env (helper z x) 
  |_-> raise(TypeError("Not a record")))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
|Def(v,x) -> let newenv = extend_tmp env v in let ex = eval_expr newenv x in 
let aaa = update newenv v ex in (newenv, Some(ex))
|Expr(x) -> (env, Some(eval_expr env x))
|NoOp -> (env, None)
