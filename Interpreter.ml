(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option


let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in
  loop cs ls

let keyword (s : string) : unit parser = literal s >> ws >| ()

(* end of parser combinators *)





(* TODO *)


type constant =
  | Name of string
  | Bool of bool
  | Int of int
  | Unit  
  | Col of (env* string * string * commands list)

and col = (env* constant * constant * commands list)

and env = (string * constant) list

and commands = 
  | Push of constant
  | Pop of int
  | Trace of int
  | Add of int
  | Sub of int
  | Mul of int
  | Div of int
  | And 
  | Or 
  | Not
  | Equal
  | Lte
  | Local 
  | Global
  | Lookup
  | Begin of commands list
  | IfElse of (commands list * commands list)
  | Fun of (string * string * commands list)
  | Call
  | Try of commands list
  | Switch of (int * commands list) list
  | End


let reserved =
  [ "Push"
  ; "True"
  ; "False"
  ; "Pop"
  ; "Add"
  ; "Sub"
  ; "Mul"
  ; "Div"
  ; "Equal"
  ; "Lte"
  ; "And"
  ; "Or"
  ; "Not"
  ; "Trace"
  ; "Local"
  ; "Global"
  ; "Lookup"
  ; "Begin"
  ; "If"
  ; "Else"
  ; "Fun"
  ; "End"
  ; "Call"
  ; "Try"
  ; "Switch"
  ; "Case"
  ]

let name : string parser =
  let* c = satisfy is_alpha in
  let* cs = many (satisfy (fun c -> is_alphanum c || c = '_' || c = '\'')) in
  let s = implode (c :: cs) in
  (* if List.exists (fun x -> x = s) reserved then
     fail
     else *)
  pure s << ws

let unit_parser () =
  let* _ = keyword "()" in
  pure Unit

let int_parser () =
  (let* n = natural in
   pure (Int n) << ws)
  <|> let* _ = keyword "-" in
  let* n = natural in
  pure (Int (-n)) << ws

let true_parser () =
  let* _ = keyword "True" in
  pure true

let false_parser () =
  let* _ = keyword "False" in
  pure false

let bool_parser () =
  let* b = true_parser () <|> false_parser () in
  pure (Bool b)
let name_parser () = 
  let* n = name in
  pure(Name n)

let const_parser () = int_parser () <|> bool_parser () <|> unit_parser () <|>  name_parser ()

let rec push_parser () =
  let* _ = keyword "Push" in
  let* cst = const_parser () in
  pure (Push cst)

let rec pop_parser () =
  let* _ = keyword "Pop" in
  let* n = natural in
  pure (Pop n) << ws

and add_parser () =
  let* _ = keyword "Add" in
  let* n = natural in
  pure (Add n) << ws

and sub_parser () =
  let* _ = keyword "Sub" in
  let* n = natural in
  pure (Sub n) << ws

and mul_parser () =
  let* _ = keyword "Mul" in
  let* n = natural in
  pure (Mul n) << ws

and div_parser () =
  let* _ = keyword "Div" in
  let* n = natural in
  pure (Div n) << ws

and trace_parser () =
  let* _ = keyword "Trace" in
  let* n = natural in
  pure (Trace n) << ws

and and_parser () =
  let* _ = keyword "And" in
  pure (And) << ws

and or_parser () =
  let* _ = keyword "Or" in
  pure (Or) << ws

and not_parser () =
  let* _ = keyword "Not" in
  pure (Not) << ws

and equal_parser () =
  let* _ = keyword "Equal" in
  pure (Equal) << ws
and lte_parser () =
  let* _ = keyword "Lte" in
  pure (Lte) << ws

and local_parser () =
  let* _ = keyword "Local" in
  pure (Local) << ws
and global_parser () =
  let* _ = keyword "Global" in
  pure (Global) << ws
and lookup_parser () =
  let* _ = keyword "Lookup" in
  pure (Lookup) << ws

and begin_parser () =
  let* _ = keyword "Begin" in
  let* ls = cmds_parser () in
  let* _ = keyword "End" in
  pure (Begin(ls)) << ws

and end_parser () =
  let* _ = keyword "End" in
  pure (End) << ws

and ifelse_parser () =
  let* _ = keyword "If" in
  let* l1 = cmds_parser () in
  let* _ = keyword "Else" in
  let* l2 = cmds_parser () in
  let* _ = keyword "End" in
  pure (IfElse(l1,l2)) << ws

and fun_parser ()= 
  let* _ = keyword "Fun" in
  let* n1 = name  in
  let* n2 = name in
  let* n3 = cmds_parser () in
  let* _ = keyword "End" in
  pure(Fun(n1,n2,n3)) << ws
and call_parser () =
  let* _ = keyword "Call" in
  pure (Call) << ws

and try_parser () =
  let* _ = keyword "Try" in
  let* ls = cmds_parser () in
  let* _ = keyword "End" in
  pure (Try(ls)) << ws

and case_parser () = 
  let* _ = keyword "Case" << ws in
  let* n = natural << ws in
  let* cmds = cmds_parser () << ws in
  pure (n,cmds) << ws

and switch_parser ()= 
  let* _ = keyword "Switch" << ws in
  let* ls = many (case_parser ()) << ws in
  let* _ = keyword "End" in
  pure (Switch(ls)) << ws

and cmd_parser () =
  push_parser () <|> pop_parser () <|> trace_parser () <|> add_parser ()
  <|> sub_parser () <|> mul_parser () <|> div_parser () <|> and_parser () 
  <|> or_parser () <|> not_parser () <|> equal_parser () <|> lte_parser ()
  <|> local_parser () <|> global_parser () <|> lookup_parser () <|> begin_parser ()
  <|> ifelse_parser () <|> fun_parser () <|> call_parser () <|> try_parser() <|> switch_parser ()

and cmds_parser () = many (cmd_parser ())

let parse_cmds s = parse (ws >> cmds_parser ()) s

let helper_parse_all (s : string) : commands list =
  match parse_cmds s with
  | Some (cmds,rest) -> cmds
  | None -> []

let rec pop (ls : constant list) (counter : int) : constant list = 
  match counter,ls with
  | 0,ls -> ls 
  | c ,h::t -> pop t (c - 1)
  | c, [] -> [] (*impossible*)

let rec helper_poped_items (ls : constant list) (counter : int) : constant list = 
  if counter = 0 then []
  else
    match ls with
    | h::t -> h :: helper_poped_items t (counter-1)
    | [] -> [] 

let rec trace (ls : constant list) (counter : int) (output : string list) : string list = 
  (List.rev (List.map (fun x -> match x with | Bool(true)-> "True" | Bool(false)-> "False" | Int(n) -> string_of_int n | Unit -> "()"  | Name(str)-> str | _ -> "Erorr idiot") (helper_poped_items ls counter)))@output

let add (ls : constant list) (counter : int) : constant list =
  Int(List.fold_left (fun x y -> match y with | Int(n) -> n + x | _-> 0) 0 (helper_poped_items ls counter))::pop ls counter

let mul (ls : constant list) (counter : int) : constant list =
  Int(List.fold_left (fun x y -> match y with | Int(n) -> n * x | _-> 0) 1 (helper_poped_items ls counter))::pop ls counter

let sub (ls : constant list) (counter : int) : constant list =
  match helper_poped_items ls counter with
  | Int(x)::t -> Int(List.fold_left (fun x y -> match y with | Int(n) -> x - n | _-> 0) (x*2) (Int(x)::t))::pop ls counter
  | _ -> Int(0):: pop ls counter 

let div (ls : constant list) (counter : int) : constant list =
  match helper_poped_items ls counter with
  | Int(x)::t -> Int(List.fold_left (fun x y -> match y with | Int(n) -> x/n | _-> 0) x t)::pop ls counter
  | _ -> Int(1):: pop ls counter 

let and_helper (ls : constant list) (counter : int) : constant list = 
  match helper_poped_items ls counter with
  | Bool(h1)::Bool(h2)::t -> Bool(h1 && h2)::(pop ls counter)
  | _ -> []

let or_helper (ls : constant list) (counter : int) : constant list = 
  match helper_poped_items ls counter with
  | Bool(h1)::Bool(h2)::t -> Bool(h1 || h2)::(pop ls counter)
  | _ -> []

let not_helper (ls : constant list) (counter : int) : constant list = 
  match helper_poped_items ls counter with
  | Bool(h1)::t -> Bool(not h1)::(pop ls counter)
  | _ -> []

let equal_helper (ls : constant list) (counter : int) : constant list = 
  match helper_poped_items ls counter with
  | Int(h1)::Int(h2)::t -> Bool(h1 = h2)::(pop ls counter)
  | _ -> []

let lte_helper (ls : constant list) (counter : int) : constant list = 
  match helper_poped_items ls counter with
  | Int(h1)::Int(h2)::t -> Bool(h1 <= h2)::(pop ls counter)
  | _ -> []

let local_helper (ls : constant list) (env_ls : env) : env =
  match ls with
  | Name(str)::h::t -> (str,h)::env_ls
  | _ -> [] (*impossible*)

let rec helper_search_env (s : string) (ls : env) : constant option = 
  match ls with
  | (str,value)::t -> if str = s then Some (value) else helper_search_env s t
  | [] -> None 

let lookup_helper (ls : constant list) (env_local : env) (env_global : env) : constant list  =
  match ls with
  | Name(str)::t -> 
    (match (helper_search_env str env_local),(helper_search_env str env_global) with
     | Some(v1),Some(v2) -> v1::t
     | Some(v1),None -> v1::t
     | None, Some(v2) -> v2::t
     | None, None -> t)
  | _ -> [] (*impossible*)

let rec check_non_int (ls : constant list) (counter : int) : bool =
  match ls, counter with 
  | _, 0 -> true
  | Int(x)::t, counter -> check_non_int t (counter-1)
  | s::t, counter -> false
  | _,_-> false

let rec check_non_bool (ls : constant list) (counter : int) : bool =
  match ls, counter with 
  | _, 0 -> true
  | Bool(x)::t, counter -> check_non_bool t (counter-1)
  | s::t, counter -> false
  | _,_-> false

let rec check_non_name (ls : constant list) : bool =
  match ls with 
  | Name(x)::t -> true
  | _ -> false

let check_zero (ls : constant list) (counter : int) : bool = 
  if counter = 0 then true
  else 
    let rec aux ls counter =
      match ls, counter with 
      | _, 0 -> true
      | Int(0)::t, counter -> false
      | Int(x)::t, counter -> aux t (counter-1)
      | _,_-> true
    in match ls with | x::[] -> true | x::t -> aux t (counter-1) | [] -> true

let check_lookup (ls : constant list) (env_ls : env) =
  let rec aux (s : string) (ls : env) =
    match ls with 
    | (str,value)::t -> if str = s then true else aux s t
    | [] -> false
  in match ls with | Name(str)::t -> aux str env_ls| _ -> false

let check_case (ls : (int * commands list) list) (stack: constant list) =
  let rec aux (s : int) (ls : (int * commands list) list) =
    match ls with 
    | (n,value)::t -> if n = s then true else aux s t
    | [] -> false
  in match stack with | Int(n)::t -> aux n ls| _ -> false

let check_switch (ls : (int * commands list) list) (stack: constant list) =
  let rec aux (s : int) (ls : (int * commands list) list) =
    match ls with 
    | (n,value)::t -> if n = s then value else aux s t
    | [] -> []
  in match stack with | Int(n)::t -> aux n ls| _ -> []
let helper_ifelse (stack : constant list) = 
  match stack with
  | Bool(x)::t -> x
  | _ -> false (*Impossible*)

let check_col (ls : constant list) = 
  match ls with
  | Col(_,_,_,_)::Int(x)::t-> true
  | Col(_,_,_,_)::Bool(x)::t-> true
  | Col(_,_,_,_)::Unit::t-> true
  | _-> false

let rec begin_helper (ls : commands list)  (local : env) (global : env) = 
  match eval ls local global with
  | (stack,output,local,global) -> (helper_poped_items stack 1,output,local,global)
and call_helper (stack : constant list) (local : env) (global : env) = 
  match helper_poped_items stack 2 with
  | Col(env,str1,str2,ls)::x::t-> (match begin_helper ls ((str1,Col(env,str1,str2,ls))::(str2,x)::env) global with | (stack,output,local,global) -> (stack,output,local,global))
  | _ -> ([],["Error"],[],[])

and  eval (ls : commands list)  (local : env) (global : env) : (constant list * string list * env * env) =  
  let rec aux (ls : commands list) (stack : constant list) (output : string list) (local : env) (global : env) : (constant list * string list * env * env) =
    match ls with
    | Push(x)::t -> aux t (x::stack) output local global
    | Pop(x)::t -> if List.length stack >= x then aux t (pop stack x) output local global else (stack,["Error"],local,global)
    | Trace(x)::t -> if List.length stack >= x then aux t (pop stack x) (trace stack x output) local global else (stack,["Error"],local,global)
    | Add(x)::t -> if List.length stack >= x && (check_non_int stack x) then aux t (add stack x) output local global else (stack,["Error"],local,global)
    | Sub(x)::t -> if List.length stack >= x && (check_non_int stack x) then aux t (sub stack x) output local global else (stack,["Error"],local,global)
    | Mul(x)::t -> if List.length stack >= x && (check_non_int stack x)then aux t (mul stack x) output  local global else (stack,["Error"],local,global)
    | Div(x)::t -> if List.length stack >= x && (check_zero stack x) && (check_non_int stack x) then aux t (div stack x) output local global else (stack,["Error"],local,global)
    | Or::t -> if List.length stack >= 2 && (check_non_bool stack 2) then aux t (or_helper stack 2) output local global else (stack,["Error"],local,global)
    | And::t -> if List.length stack >= 2 && (check_non_bool stack 2) then aux t (and_helper stack 2) output local global else (stack,["Error"],local,global)
    | Not::t -> if List.length stack >= 1 && (check_non_bool stack 1) then aux t (not_helper stack 1) output local global else (stack,["Error"],local,global)
    | Equal::t -> if List.length stack >= 2 && (check_non_int stack 2) then aux t (equal_helper stack 2) output local global else (stack,["Error"],local,global)
    | Lte::t -> if List.length stack >= 2 && (check_non_int stack 2) then aux t (lte_helper stack 2) output local global else (stack,["Error"],local,global)
    | Local::t -> if List.length stack >= 2 && (check_non_name stack) then aux t (Unit::(pop stack 2)) output (local_helper stack local) global  else (stack,["Error"],local,global)
    | Global::t -> if List.length stack >= 2 && (check_non_name stack) then aux t (Unit::(pop stack 2)) output local (local_helper stack global)  else (stack,["Error"],local,global)
    | Lookup::t -> if List.length stack >= 1 && (check_non_name stack) && (check_lookup stack local || check_lookup stack global) then aux t (lookup_helper stack local global) output local global else (stack,["Error"],local,global)
    | Begin(ls)::t -> (match begin_helper ls local global with 
        | (s,["Error"],l,g) -> (stack,["Error"],local,global)
        | ([],o,l,g) -> (stack,["Error"],local,global)
        | (s,o,l,g) -> aux t (s@stack) (o@output) local g)
    | IfElse(l1,l2)::t -> 
      if List.length stack >= 1 && (check_non_bool stack 1) then 
        if (helper_ifelse stack) then 
          aux (l1@t) (pop stack 1) output local global
        else
          aux (l2@t) (pop stack 1) output local global
      else ([],["Error"],[],[]) 
    | Fun(str1,str2,ls)::t -> aux t stack output ((str1,Col(local,str1,str2,ls))::local) global
    | Call::t -> 
      (match call_helper stack local global with  
       | (s,o,l,g) -> aux t (s@(pop stack 2)) (o@output) local g)
    | Try(ls)::t -> 
      (match begin_helper ls local global with 
       | (s,["Error"],l,g) -> aux t stack output local g
       | ([],o,l,g) -> (stack,["Error"],local,global)
       | (s,o,l,g) -> aux t (s@stack) (o@output) local g)
    | Switch(ls)::t -> 
      if (check_non_int stack 1) && check_case ls stack then 
        aux ((check_switch ls stack)@t) (pop stack 1) output local global
      else (stack,["Error"],local,global)
    | _ -> (stack,output,[],global)
  in aux ls [] [] local global

let interp (src : string)  =  
  match eval (helper_parse_all src) [] [] with
  | (stack,output,local,global) -> output

(* call interp with any string of commands to interpret*)

(* Example: 

   "Begin
   Fun vloop vn
    Begin
      Push vn
      Lookup
      Push 39
      Div 2
      Trace 1
      Push ()
      Push v_
      Local
      Pop 1
      Push 3
      Push vn
      Lookup
      Div 2
      Push vloop
      Lookup
      Call
    End
   End
   Push vloop
   Lookup
   Push vloop
   Local
   Pop 1
   Push False
   Push exn
   Global
   Pop 1
   Try
    Push 39
    Push vloop
    Lookup
    Call
    Push True
    Push exn
    Global
    Pop 1
   End
   Push exn
   Lookup
   If
    Pop 0
   Else
    Begin
      Push False
      Trace 1
      Push ()
      Push True
      Push exn
      Global
      Pop 1
    End
   End
   End" *)





