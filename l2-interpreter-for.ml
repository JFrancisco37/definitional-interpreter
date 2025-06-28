(* ============== DEFINIÇÕES DE TIPOS E DADOS ============== *)

(* NOVO: Adicionado Leq para o for loop *)
type bop = Sum | Sub | Mul | Div | Eq | Gt | Lt | Leq | Neq | And | Or

type tipo =
 | TyInt
 | TyBool
 | TyRef of tipo
 | TyUnit

(* NOVO: Adicionado o construtor For *)
type expr =
 | Num of int
 | Bool of bool
 | Id of string
 | Binop of bop * expr * expr
 | If of expr * expr * expr
 | Let of string * tipo * expr * expr
 | Atrib of expr * expr
 | Deref of expr
 | New of expr
 | Unit
 | While of expr * expr
 | Seq of expr * expr
 | Read
 | Print of expr
 | Loc of int
 | For of string * expr * expr * expr (* for i = e1 to e2 do e3 *)

type state = {
 mem: (int * expr) list;
 input: int list;
 output: int list;
 next_loc: int;
}

type tyEnv = (string * tipo) list

exception TypeError of string
exception BugParser of string
exception NoRuleApplies
exception DivZero

(* ============== VERIFICADOR DE TIPOS (TYPEINFER) ============== *)

let rec lookup (g:tyEnv) (x:string) : tipo option =
 match g with
  [] -> None
 | (y,t):: tail -> if x=y then Some t else lookup tail x

let rec typeinfer (g:tyEnv) (e:expr) : tipo =
 match e with
 | Num _ -> TyInt
 | Bool _ -> TyBool
 | Unit -> TyUnit
 | Loc _ -> raise (BugParser "Localizações não devem aparecer no código fonte")

 | Id x -> (match lookup g x with
   | None -> raise (TypeError ("Identificador não declarado: " ^ x))
   | Some t -> t)

 | Binop(o,e1,e2) ->
   let t1 = typeinfer g e1 in
   let t2 = typeinfer g e2 in
   (match o with
    Sum | Sub | Mul | Div ->
     if (t1 = TyInt) && (t2 = TyInt) then TyInt
     else raise (TypeError "Operandos de operações aritméticas devem ser inteiros")
   (* NOVO: Regra de tipo para Leq *)
   | Eq | Gt | Lt | Leq | Neq ->
     if (t1 = TyInt) && (t2 = TyInt) then TyBool
     else raise (TypeError "Operandos de operações relacionais devem ser inteiros")
   | And | Or ->
     if (t1 = TyBool) && (t2 = TyBool) then TyBool
     else raise (TypeError "Operandos de operações booleanas devem ser booleanos"))

 | If(e1,e2,e3) ->
   let t1 = typeinfer g e1 in
   if (t1 = TyBool) then
    let t2 = typeinfer g e2 in
    let t3 = typeinfer g e3 in
    if (t2 = t3) then t2
    else raise (TypeError "Expressões then/else devem ter o mesmo tipo")
   else raise (TypeError "Condição do if-then-else deve ser booleana")

 | Let(x,t,e1,e2) ->
   let t1 = typeinfer g e1 in
   if t1 = t then
    let g' = (x,t)::g in
    typeinfer g' e2
   else raise (TypeError "Expressão associada a um id deve ter o tipo declarado")

 | Atrib(e1, e2) ->
   let t1 = typeinfer g e1 in
   let t2 = typeinfer g e2 in
   (match t1 with
   | TyRef t -> if t = t2 then TyUnit else raise (TypeError "Tipo da expressão incompatível com o tipo da referência")
   | _ -> raise (TypeError "Atribuição requer uma referência no lado esquerdo"))

 | Deref(e1) ->
   let t1 = typeinfer g e1 in
   (match t1 with
   | TyRef t -> t
   | _ -> raise (TypeError "Derreferência (!) espera uma referência"))

 | New(e1) -> TyRef (typeinfer g e1)

 | Seq(e1, e2) ->
   let t1 = typeinfer g e1 in
   if t1 = TyUnit then typeinfer g e2
   else raise (TypeError "O lado esquerdo de uma sequência (;) deve ter tipo unit")

 | While(e1, e2) ->
   let t1 = typeinfer g e1 in
   let t2 = typeinfer g e2 in
   if t1 <> TyBool then raise (TypeError "A condição do while deve ser booleana");
   if t2 <> TyUnit then raise (TypeError "O corpo do while deve ter tipo unit");
   TyUnit

 (* NOVO: Regra de tipo para o for *)
 | For(i, e_start, e_end, e_body) ->
   if typeinfer g e_start <> TyInt then
    raise (TypeError "Valor inicial do for deve ser int.");
   if typeinfer g e_end <> TyInt then
    raise (TypeError "Valor final do for deve ser int.");

   let g' = (i, TyInt) :: g in
   if typeinfer g' e_body <> TyUnit then
    raise (TypeError "Corpo do for deve ter tipo unit.");
   TyUnit

 | Read -> TyInt
 | Print(e1) ->
   if typeinfer g e1 = TyInt then TyUnit
   else raise (TypeError "Print espera uma expressão do tipo int")


(* ============== AVALIADOR SMALL-STEP ============== *)

let is_value (e:expr) : bool =
 match e with
 | Num _ | Bool _ | Unit | Loc _ -> true
 | _ -> false

let rec subs (v:expr) (x:string) (e:expr) : expr =
 match e with
 | Id y -> if x=y then v else e
 | Binop(o,e1,e2) -> Binop(o, subs v x e1, subs v x e2)
 | If(e1,e2,e3) -> If(subs v x e1, subs v x e2, subs v x e3)
 | Let(y,t,e1,e2) ->
   if x=y then Let(y,t,subs v x e1, e2)
   else Let(y,t,subs v x e1, subs v x e2)
 | Atrib(e1,e2) -> Atrib(subs v x e1, subs v x e2)
 | Deref(e1) -> Deref(subs v x e1)
 | New(e1) -> New(subs v x e1)
 | Seq(e1,e2) -> Seq(subs v x e1, subs v x e2)
 | While(e1,e2) -> While(subs v x e1, subs v x e2)
 | Print(e1) -> Print(subs v x e1)
 | For(j, e1, e2, e3) -> if x=j then e else For(j, subs v x e1, subs v x e2, subs v x e3)
 | _ -> e

(* NOVO: Adicionado Leq *)
let compute (o:bop) (v1:expr) (v2:expr) : expr =
 match (o, v1, v2) with
 | (Sum, Num n1, Num n2) -> Num (n1 + n2)
 | (Sub, Num n1, Num n2) -> Num (n1 - n2)
 | (Mul, Num n1, Num n2) -> Num (n1 * n2)
 | (Div, Num n1, Num n2) -> if n2 <> 0 then Num (n1 / n2) else raise DivZero
 | (Lt, Num n1, Num n2) -> Bool (n1 < n2)
 | (Gt, Num n1, Num n2) -> Bool (n1 > n2)
 | (Leq, Num n1, Num n2) -> Bool (n1 <= n2)
 | (Eq, Num n1, Num n2) -> Bool (n1 = n2)
 | (Neq, Num n1, Num n2) -> Bool (n1 <> n2)
 | (And, Bool b1, Bool b2) -> Bool (b1 && b2)
 | (Or, Bool b1, Bool b2) -> Bool (b1 || b2)
 | _ -> raise NoRuleApplies

(* CORRIGIDO: A função agora aceita uma tupla como argumento *)
let rec step ((e, s): expr * state) : (expr * state) =
 match e with
 (* NOVO: Regra de dessalinização para o for *)
 | For(i, e_start, e_end, e_body) ->
   let counter_id = "_counter_" ^ i in
   let end_id = "_end_" ^ i in
   let desugared =
    Let(counter_id, TyRef TyInt, New e_start,
     Let(end_id, TyInt, e_end,
      While(
       Binop(Leq, Deref (Id counter_id), Id end_id),
       Seq(
        Let(i, TyInt, Deref (Id counter_id), e_body),
        Atrib(Id counter_id, Binop(Sum, Deref (Id counter_id), Num 1))
       )
      )
     )
    )
   in (desugared, s)

 | Binop(o, e1, e2) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (Binop(o, e1', e2), s')
 | Binop(o, v1, e2) when is_value v1 && not (is_value e2) ->
   let (e2', s') = step (e2, s) in (Binop(o, v1, e2'), s')
 | If(e1, e2, e3) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (If(e1', e2, e3), s')
 | Let(x, t, e1, e2) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (Let(x, t, e1', e2), s')
 | Atrib(e1, e2) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (Atrib(e1', e2), s')
 | Atrib(v1, e2) when is_value v1 && not (is_value e2) ->
   let (e2', s') = step (e2, s) in (Atrib(v1, e2'), s')
 | Deref(e1) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (Deref e1', s')
 | New(e1) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (New e1', s')
 | Seq(e1, e2) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (Seq(e1', e2), s')
 | Print(e1) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (Print e1', s')
   
 | Binop(o, v1, v2) when is_value v1 && is_value v2 -> (compute o v1 v2, s)
 | If(Bool true, e2, e3) -> (e2, s)
 | If(Bool false, e2, e3) -> (e3, s)
 | Let(x, t, v1, e2) when is_value v1 -> (subs v1 x e2, s)
 
 | Atrib(Loc l, v) when is_value v ->
   let new_mem = (l, v) :: List.remove_assoc l s.mem in
   (Unit, {s with mem = new_mem})
 
 | Deref(Loc l) ->
   (match List.assoc_opt l s.mem with
   | Some v -> (v, s)
   | None -> raise (BugParser ("Endereço não encontrado na memória: " ^ (string_of_int l))))
   
 | New v when is_value v ->
   let l = s.next_loc in
   let new_mem = (l, v) :: s.mem in
   (Loc l, {s with mem = new_mem; next_loc = l + 1})

 | Seq(Unit, e2) -> (e2, s)
 
 | While(e1, e2) -> (If(e1, Seq(e2, While(e1, e2)), Unit), s)

 | Read ->
   (match s.input with
   | [] -> raise (BugParser "Entrada (input) vazia")
   | h::t -> (Num h, {s with input = t}))

 | Print(Num n) ->
   (Unit, {s with output = s.output @ [n]})

 | _ -> raise NoRuleApplies

(* CORRIGIDO: A função agora aceita uma tupla como argumento *)
let rec eval ((e, s): expr * state) : (expr * state) =
 if is_value e then (e, s)
 else
  try
   let (e', s') = step (e, s) in
   (* A chamada recursiva agora está correta pois a definição de eval foi corrigida *)
   eval (e', s')
  with e -> raise e (* Apenas para garantir que exceções não sejam engolidas *)


(* ============== INTERPRETADOR PRINCIPAL E TESTES ============== *)

let rec strofvalue (v:expr) : string =
 match v with
 | Num n -> string_of_int n
 | Bool b -> string_of_bool b
 | Unit -> "()"
 | Loc l -> "loc<" ^ (string_of_int l) ^ ">"
 | _  -> raise (BugParser "Não é um valor")
  
let rec stroftipo (t:tipo) : string =
 match t with
 | TyInt -> "int"
 | TyBool -> "bool"
 | TyRef t1 -> "ref " ^ (stroftipo t1)
 | TyUnit -> "unit"

let print_output (out: int list) : unit =
 let rec p o =
  match o with
  | [] -> ()
  | h::t -> print_int h; print_string "; "; p t
 in
 print_string "["; p out; print_string "]"

let inter (name: string) (e:expr) (i:int list) : unit =
 try
  print_endline ("-- Executando teste: " ^ name ^ " --");
  print_string "Verificando tipos...\n";
  let t = typeinfer [] e in
  print_string ("Expressão bem tipada. Tipo inferido: " ^ (stroftipo t) ^ "\n");
  
  print_string "Avaliando...\n";
  let initial_state = { mem = []; input = i; output = []; next_loc = 0 } in
  let (v, final_state) = eval (e, initial_state) in
  
  print_string "========================================\n";
  print_string ("Valor final: " ^ (strofvalue v) ^ " : " ^ (stroftipo t) ^ "\n");
  print_string "Saída (output): ";
  print_output final_state.output;
  print_string "\n========================================\n\n"

 with
 | TypeError msg -> print_endline ("\nERRO DE TIPO: " ^ msg)
 | BugParser msg -> print_endline ("\nERRO DE EXECUÇÃO (BUG): " ^ msg)
 | DivZero -> print_endline ("\nERRO DE EXECUÇÃO: Divisão por zero.")
 | NoRuleApplies -> print_endline ("\nERRO DE EXECUÇÃO (BUG): Nenhuma regra de avaliação se aplica.")


(* ============== TESTES ============== *)

(* Teste Fatorial com 'while' (original) *)
let fat_test =
 Let("x", TyInt, Read,
  Let("z", TyRef TyInt, New (Id "x"),
   Let("y", TyRef TyInt, New (Num 1),
    Seq(
     While(
      Binop(Gt, Deref (Id "z"), Num 0),
      Seq(
       Atrib(Id "y", Binop(Mul, Deref (Id "y"), Deref (Id "z"))),
       Atrib(Id "z", Binop(Sub, Deref (Id "z"), Num 1))
      )
     ),
     Print(Deref (Id "y"))
    )
   )
  )
 )

(* NOVO: Teste com o laço 'for' *)
(*
 let sum: ref int = new 0 in
 for i = 1 to 5 do (
  sum := !sum + i;
  print(i)
 );
 print(!sum)
*)
let for_test =
 Let("sum", TyRef TyInt, New (Num 0),
  Seq(
   For("i", Num 1, Num 5,
    Seq(
     Atrib(Id "sum", Binop(Sum, Deref(Id "sum"), Id "i")),
     Print(Id "i")
    )
   ),
   Print(Deref (Id "sum"))
  )
 )

let () =
 inter "Fatorial com while" fat_test [5];
 inter "Soma e contagem com for" for_test []