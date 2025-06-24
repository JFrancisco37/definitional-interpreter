(* ============== DEFINIÇÕES DE TIPOS E DADOS ============== *)

(* Operadores binários. No mínimo Sum e Lt são necessários.  *)
type bop = Sum | Sub | Mul | Div | Eq | Gt | Lt | Neq | And | Or

(* Tipos da linguagem L2.  *)
type tipo = 
  | TyInt
  | TyBool
  | TyRef of tipo
  | TyUnit

(* Expressões da linguagem L2.  *)
(* Adicionado Loc para representar endereços que são valores intermediários.  *)
type expr =
  | Num of int
  | Bool of bool
  | Id of string
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Let of string * tipo * expr * expr
  | Atrib of expr * expr      (* e1 := e2 *)
  | Deref of expr         (* !e *)
  | New of expr
  | Unit                  (* () *)
  | While of expr * expr
  | Seq of expr * expr        (* e1; e2 *)
  | Read
  | Print of expr
  | Loc of int              (* Localização na memória, um valor. *)

(* O estado da máquina: memória, canais de entrada/saída e próximo local livre. *)
type state = {
  mem: (int * expr) list;
  input: int list;
  output: int list;
  next_loc: int;
}

(* Ambiente de tipagem. *)
type tyEnv = (string * tipo) list

exception TypeError of string
exception BugParser of string
exception NoRuleApplies
exception DivZero

(* ============== VERIFICADOR DE TIPOS (TYPEINFER) ============== *)

let rec lookup (g:tyEnv) (x:string) : tipo option = 
  match g with 
    [] -> None
  | (y,t):: tail -> if x=y then Some t else lookup tail x [cite: 9]

let rec typeinfer (g:tyEnv) (e:expr) : tipo =
  match e with
  | Num _ -> TyInt
  | Bool _ -> TyBool [cite: 11]
  | Unit -> TyUnit [cite: 84]
  | Loc _ -> raise (BugParser "Localizações não devem aparecer no código fonte")

  | Id x -> (match lookup g x with
      | None -> raise (TypeError ("Identificador não declarado: " ^ x)) [cite: 15]
      | Some t -> t)

  | Binop(o,e1,e2) ->
      let t1 = typeinfer g e1 in
      let t2 = typeinfer g e2 in
      (match o with
         Sum | Sub | Mul | Div ->
           if (t1 = TyInt) && (t2 = TyInt) then TyInt
           else raise (TypeError "Operandos de operações aritméticas devem ser inteiros")
       | Eq | Gt | Lt | Neq ->
           if (t1 = TyInt) && (t2 = TyInt) then TyBool [cite: 13]
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
      else raise (TypeError "Condição do if-then-else deve ser booleana") [cite: 14]

  | Let(x,t,e1,e2) -> 
      let t1 = typeinfer g e1 in
      if t1 = t then
        let g' = (x,t)::g in
        typeinfer g' e2
      else raise (TypeError "Expressão associada a um id deve ter o tipo declarado") [cite: 17]
  
  (* Regras de tipo para L2  *)
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
      if t1 = TyUnit then typeinfer g e2 [cite: 85, 86]
      else raise (TypeError "O lado esquerdo de uma sequência (;) deve ter tipo unit")

  | While(e1, e2) ->
      let t1 = typeinfer g e1 in
      let t2 = typeinfer g e2 in
      if t1 <> TyBool then raise (TypeError "A condição do while deve ser booleana")[cite: 87];
      if t2 <> TyUnit then raise (TypeError "O corpo do while deve ter tipo unit")[cite: 87];
      TyUnit

  (* Regras de tipo para I/O  *)
  | Read -> TyInt
  | Print(e1) ->
      if typeinfer g e1 = TyInt then TyUnit
      else raise (TypeError "Print espera uma expressão do tipo int")


(* ============== AVALIADOR SMALL-STEP ============== *)

(* Verifica se uma expressão é um valor final. *)
let is_value (e:expr) : bool =
  match e with
  | Num _ | Bool _ | Unit | Loc _ -> true
  | _ -> false

(* Substituição de identificadores por valores. *)
let rec subs (v:expr) (x:string) (e:expr) : expr = 
  match e with
  | Id y -> if x=y then v else e [cite: 25]
  | Binop(o,e1,e2) -> Binop(o, subs v x e1, subs v x e2) [cite: 23]
  | If(e1,e2,e3) -> If(subs v x e1, subs v x e2, subs v x e3) [cite: 24]
  | Let(y,t,e1,e2) -> 
      if x=y then Let(y,t,subs v x e1, e2) 
      else Let(y,t,subs v x e1, subs v x e2) [cite: 27]
  | Atrib(e1,e2) -> Atrib(subs v x e1, subs v x e2)
  | Deref(e1) -> Deref(subs v x e1)
  | New(e1) -> New(subs v x e1)
  | Seq(e1,e2) -> Seq(subs v x e1, subs v x e2)
  | While(e1,e2) -> While(subs v x e1, subs v x e2)
  | Print(e1) -> Print(subs v x e1)
  | _ -> e (* Num, Bool, Unit, Loc, Read não contêm variáveis livres. *)

(* Computa o resultado de uma operação binária com valores. *)
let compute (o:bop) (v1:expr) (v2:expr) : expr =
  match (o, v1, v2) with
  | (Sum, Num n1, Num n2) -> Num (n1 + n2)
  | (Sub, Num n1, Num n2) -> Num (n1 - n2)
  | (Mul, Num n1, Num n2) -> Num (n1 * n2) [cite: 30]
  | (Div, Num n1, Num n2) -> if n2 <> 0 then Num (n1 / n2) else raise DivZero
  | (Lt, Num n1, Num n2) -> Bool (n1 < n2)
  | (Gt, Num n1, Num n2) -> Bool (n1 > n2)
  | (Eq, Num n1, Num n2) -> Bool (n1 = n2)
  | (Neq, Num n1, Num n2) -> Bool (n1 <> n2)
  | (And, Bool b1, Bool b2) -> Bool (b1 && b2) [cite: 31]
  | (Or, Bool b1, Bool b2) -> Bool (b1 || b2) [cite: 31]
  | _ -> raise NoRuleApplies

(* A função step, coração do interpretador small-step. *)
let rec step (e:expr, s:state) : (expr * state) =
  match e with
  (* Regras de Congruência (quando um sub-termo precisa ser avaliado) *)
  | Binop(o, e1, e2) when not (is_value e1) ->
      let (e1', s') = step (e1, s) in (Binop(o, e1', e2), s') [cite: 36]
  | Binop(o, v1, e2) when is_value v1 && not (is_value e2) ->
      let (e2', s') = step (e2, s) in (Binop(o, v1, e2'), s') [cite: 35]
  | If(e1, e2, e3) when not (is_value e1) ->
      let (e1', s') = step (e1, s) in (If(e1', e2, e3), s') [cite: 38]
  | Let(x, t, e1, e2) when not (is_value e1) ->
      let (e1', s') = step (e1, s) in (Let(x, t, e1', e2), s') [cite: 41]
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
      
  (* Regras de Computação (quando os operandos são valores) *)
  | Binop(o, v1, v2) when is_value v1 && is_value v2 -> (compute o v1 v2, s) [cite: 34]
  | If(Bool true, e2, e3) -> (e2, s) [cite: 37]
  | If(Bool false, e2, e3) -> (e3, s)
  | Let(x, t, v1, e2) when is_value v1 -> (subs v1 x e2, s) [cite: 40]
  
  (* Semântica de L2  *)
  | Atrib(Loc l, v) when is_value v ->
      let new_mem = (l, v) :: List.remove_assoc l s.mem in
      (Unit, {s with mem = new_mem}) [cite: 68]
  
  | Deref(Loc l) ->
      (match List.assoc_opt l s.mem with
       | Some v -> (v, s)
       | None -> raise (BugParser ("Endereço não encontrado na memória: " ^ (string_of_int l))))
       
  | New v when is_value v ->
      let l = s.next_loc in
      let new_mem = (l, v) :: s.mem in
      (Loc l, {s with mem = new_mem; next_loc = l + 1}) [cite: 70]

  | Seq(Unit, e2) -> (e2, s) [cite: 72, 73]
  
  | While(e1, e2) -> (If(e1, Seq(e2, While(e1, e2)), Unit), s)

  | Read ->
      (match s.input with
       | [] -> raise (BugParser "Entrada (input) vazia")
       | h::t -> (Num h, {s with input = t}))

  | Print(Num n) ->
      (Unit, {s with output = s.output @ [n]})

  (* Se nenhuma regra se aplica, a expressão é um valor ou um erro. *)
  | _ -> raise NoRuleApplies


(* Executa a avaliação small-step repetidamente. *)
let rec eval (e:expr, s:state) : (expr * state) =
  if is_value e then (e, s)
  else
    try
      let (e', s') = step (e, s) in
      eval (e', s')
    with
      NoRuleApplies -> (e, s) (* Para quando `step` não se aplica mais *)

(* ============== INTERPRETADOR PRINCIPAL ============== *)

let rec strofvalue (v:expr) : string = 
  match v with 
  | Num n -> string_of_int n [cite: 47]
  | Bool b -> string_of_bool b [cite: 47]
  | Unit -> "()"
  | Loc l -> "loc<" ^ (string_of_int l) ^ ">"
  | _    -> raise (BugParser "Não é um valor")
    
let rec stroftipo (t:tipo) : string = 
  match t with
  | TyInt -> "int" [cite: 49]
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

let inter (e:expr) (i:int list) : unit = 
  try
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


(* ============== TESTE: FATORIAL IMPERATIVO ============== *)

(*
  O código original (com açúcar sintático):
  
  let x: int = read() in          // ex: input = 5
  let z: ref int = new x in 
  let y: ref int = new 1 in 
  
  while (!z > 0) (
    y := !y * !z;
    z := !z - 1
  );
  
  print(!y)
  
  AST correspondente, usando os datatypes definidos. 
*)

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

(* Para executar o teste: *)
let () =
  print_endline "Executando teste do fatorial...";
  inter fat_test [5] (* Calcula 5! *)