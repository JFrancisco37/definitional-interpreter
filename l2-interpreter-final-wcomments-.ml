(*
  ============================================================================
  PARTE 1: DEFININDO A GRAMÁTICA E OS TIPOS DA LINGUAGEM
  --------------------------------------------------
  Um interpretador é um programa que entende outra linguagem. Antes de podermos
  escrever a lógica para *executar* o código, primeiro precisamos de uma maneira de
  representar esse código como dados dentro do nosso programa OCaml. Isso é como
  definir a gramática da linguagem. Usamos o poderoso sistema de `tipos` do OCaml
  para isso.
  ============================================================================
*)

(*
  O QUE É:
  Um tipo "variante" chamado `bop` que lista todos os operadores binários
  possíveis em nossa linguagem simples.

  SINTAXE OCAML:
  - `type bop = ...`: Declara um novo tipo de dado chamado `bop`.
  - `| Sum | Sub | ...`: Estes são os "construtores" para o tipo. Um valor do
    tipo `bop` pode ser APENAS um destes. Pense nisso como um `enum` em
    outras linguagens.
*)
type bop = Sum | Sub | Mul | Div | Eq | Gt | Lt | Leq | Neq | And | Or

(*
  O QUE É:
  Isso define o sistema de tipos *da linguagem que estamos interpretando*. Nossa
  linguagem simples terá inteiros, booleanos, referências (como ponteiros) e um
  tipo especial `unit`.

  SINTAXE OCAML:
  - `TyRef of tipo`: Este é um construtor que *carrega um dado*. `TyRef` não é
    apenas um valor; é um "invólucro". Por exemplo, para representar o tipo
    "referência a um inteiro", escreveríamos `TyRef TyInt`.
  - Esta é uma *definição de tipo recursiva*, pois `TyRef` se refere de volta a `tipo`.
*)
type tipo =
 | TyInt         (* O tipo para inteiros, como `5`. *)
 | TyBool        (* O tipo para booleanos, como `true`. *)
 | TyRef of tipo (* O tipo para referências (ponteiros). Ex: `TyRef TyInt` é o tipo de um ponteiro para um inteiro. *)
 | TyUnit        (* Um tipo com apenas um valor, `()`. É como `void` em C/Java, usado quando uma expressão não tem um resultado significativo. *)

(*
  O QUE É:
  Esta é a definição de tipo mais importante, a Árvore de Sintaxe Abstrata (AST).
  A AST é como nosso programa OCaml representa o código da nossa linguagem simples
  como um dado estruturado. Um programa inteiro é apenas uma grande `expr`.
  Por exemplo, o código `1 + 2` seria representado pela estrutura de dados:
  `Binop(Sum, Num 1, Num 2)`.

  SINTAXE OCAML:
  - `Num of int`: O construtor `Num` carrega um valor `int` do OCaml.
  - `Binop of bop * expr * expr`: O construtor `Binop` carrega uma `tupla`.
    Uma tupla `(a, b, c)` é uma coleção de tamanho fixo de elementos de tipos
    potencialmente diferentes. Esta tupla aqui carrega o operador, a expressão da
    esquerda e a expressão da direita.
*)
type expr =
 | Num of int                  (* Representa um literal numérico, ex: `5`. *)
 | Bool of bool                (* Representa um literal booleano, ex: `true`. *)
 | Id of string                (* Representa o nome de uma variável, ex: `x`. *)
 | Binop of bop * expr * expr  (* Representa uma operação binária, ex: `e1 + e2`. *)
 | If of expr * expr * expr    (* Representa uma expressão if-then-else. *)
 | Let of string * tipo * expr * expr (* Representa `let x: t = e1 in e2`. *)
 | Atrib of expr * expr        (* Atribuição a uma referência, ex: `x := e`. *)
 | Deref of expr               (* De-referenciação de um ponteiro, ex: `!x`. *)
 | New of expr                 (* Alocação de memória para uma nova referência, ex: `new e`. *)
 | Unit                        (* O valor `unit`, escrito como `()`. *)
 | While of expr * expr        (* Um laço while, `while <condição> do <corpo>`. *)
 | Seq of expr * expr          (* Uma sequência, `e1; e2`. Executa `e1`, depois `e2`. *)
 | Read                        (* Lê um inteiro do fluxo de entrada. *)
 | Print of expr               (* Imprime uma expressão inteira. *)
 | Loc of int                  (* Representa um endereço de memória. É puramente para uso interno do interpretador. *)
 | For of string * expr * expr * expr (* Representa `for i = e1 to e2 do e3`. *)


(*
  O QUE É:
  Isso representa todo o estado do nosso "computador virtual" enquanto ele executa
  o programa. Um interpretador funciona simulando um computador, e este registro
  contém todas as partes necessárias dessa simulação: memória, E/S, etc.

  SINTAXE OCAML:
  - `type ... = { ... }`: Isso define um tipo "registro" (record), que é como uma `struct`
    em C ou uma classe simples apenas com dados em outras linguagens.
  - `mem: (int * expr) list`: O campo `mem` é uma `lista` de tuplas `(int * expr)`.
    Usamos isso para simular a memória, mapeando endereços inteiros para valores.
*)
type state = {
 mem: (int * expr) list; (* A memória do computador. *)
 input: int list;         (* Um fluxo de números a serem lidos pelo comando `Read`. *)
 output: int list;        (* Um registro de todos os números impressos pelo comando `Print`. *)
 next_loc: int;           (* Um contador para rastrear o próximo endereço de memória a ser alocado. *)
}

(*
  O QUE É:
  Um "alias de tipo". Estamos apenas dizendo que `tyEnv` é um nome mais curto e
  descritivo para `(string * tipo) list`. Este "ambiente de tipagem" é como um
  dicionário que o verificador de tipos usa para lembrar o tipo de cada variável.
  Por exemplo, `[("x", TyInt)]` significa "a variável `x` é um inteiro".
*)
type tyEnv = (string * tipo) list


(*
  ============================================================================
  PARTE 2: DEFININDO ERROS (EXCEÇÕES)
  ------------------------------------
  Bons programas precisam de um bom tratamento de erros. Definimos "exceções"
  personalizadas para representar as diferentes coisas que podem dar errado.
  ============================================================================
*)
exception TypeError of string   (* Para quando o código do usuário viola regras de tipo (ex: `"ola" + 5`). *)
exception BugParser of string   (* Para um erro interno no nosso interpretador (um bug que precisamos corrigir). *)
exception NoRuleApplies         (* Um erro interno especial para quando nosso avaliador fica "preso". *)
exception DivZero               (* Para quando o código do usuário tenta dividir por zero. *)


(*
  ============================================================================
  PARTE 3: O VERIFICADOR DE TIPOS
  ------------------------
  Esta é a primeira fase principal do nosso interpretador. Antes de executar
  qualquer código, nós o analisamos para garantir que ele é "bem tipado". Isso
  captura erros como `if (5) then ...` (porque 5 não é um booleano) antes
  mesmo do início da execução. A função principal aqui é `typeinfer`.
  ============================================================================
*)

(*
  PROPÓSITO:
  Uma função auxiliar que busca o tipo de uma variável no ambiente de tipagem.

  SINTAXE OCAML:
  - `let rec ...`: Define uma função. `rec` significa que ela é "recursiva" (pode chamar a si mesma).
  - `(g:tyEnv)`: `g` é o argumento, `:tyEnv` é sua anotação de tipo.
  - `: tipo option`: Este é o tipo de retorno da função. `option` é um tipo
    embutido para valores que podem estar ausentes. Ele pode ser `Some valor` ou `None`.
    Isso é mais seguro do que retornar `null`.
  - `match g with ...`: "Casamento de padrões" (Pattern matching). É como um `switch`
    superpoderoso que inspeciona a *estrutura* dos dados.
  - `[] -> None`: O "padrão" `[]` casa com uma lista vazia. Se a lista estiver vazia,
    a variável não foi encontrada, então retornamos `None`.
  - `(y,t)::tail -> ...`: Este padrão casa com uma lista não vazia. Ele "desestrutura"
    a lista em seu primeiro elemento `(y,t)` (a cabeça) e o resto da lista `tail`.
*)
let rec lookup (g:tyEnv) (x:string) : tipo option =
 match g with
  [] -> None
 | (y,t):: tail -> if x=y then Some t else lookup tail x

(*
  PROPÓSITO:
  A principal função de inferência de tipos (ou verificação de tipos). Ela percorre
  uma árvore `expr` e determina seu tipo. Se encontrar uma violação de tipo, ela
  lança (`raise`) um `TypeError`.
*)
let rec typeinfer (g:tyEnv) (e:expr) : tipo =
 match e with
  (* Casos base: os tipos de literais são óbvios. *)
 | Num _ -> TyInt      (* `_` é um padrão curinga, significando "não me importo qual int é, o tipo é TyInt". *)
 | Bool _ -> TyBool
 | Unit -> TyUnit
 | Loc _ -> raise (BugParser "Localizações não devem aparecer no código fonte")

  (* Para encontrar o tipo de uma variável, basta procurá-la no ambiente. *)
 | Id x -> (match lookup g x with
   | None -> raise (TypeError ("Identificador não declarado: " ^ x))
   | Some t -> t)

  (* Para verificar o tipo de uma operação binária, verificamos os tipos dos operandos. *)
 | Binop(o,e1,e2) ->
    (* SINTAXE OCAML: `let <var> = <expr> in <corpo>` define uma variável local `<var>`
       que só está disponível dentro de `<corpo>`. *)
   let t1 = typeinfer g e1 in (* Encontra recursivamente o tipo do operando esquerdo. *)
   let t2 = typeinfer g e2 in (* Encontra recursivamente o tipo do operando direito. *)
   (match o with
    | Sum | Sub | Mul | Div ->
      if (t1 = TyInt) && (t2 = TyInt) then TyInt
      else raise (TypeError "Operandos de operações aritméticas devem ser inteiros")
    | Eq | Gt | Lt | Leq | Neq ->
      if (t1 = TyInt) && (t2 = TyInt) then TyBool
      else raise (TypeError "Operandos de operações relacionais devem ser inteiros")
    | And | Or ->
      if (t1 = TyBool) && (t2 = TyBool) then TyBool
      else raise (TypeError "Operandos de operações booleanas devem ser booleanos"))

  (* Para uma expressão `if`, temos três regras: *)
 | If(e1,e2,e3) ->
   let t1 = typeinfer g e1 in
   if (t1 = TyBool) then (* Regra 1: A condição `e1` deve ser um booleano. *)
    let t2 = typeinfer g e2 in (* Tipo do ramo `then`. *)
    let t3 = typeinfer g e3 in (* Tipo do ramo `else`. *)
    if (t2 = t3) then t2       (* Regra 2: Ambos os ramos devem ter o mesmo tipo. *)
    else raise (TypeError "Expressões then/else devem ter o mesmo tipo")
   else raise (TypeError "Condição do if-then-else deve ser booleana")

  (* Para uma expressão `let`: `let x:t = e1 in e2` *)
 | Let(x,t,e1,e2) ->
   let t1 = typeinfer g e1 in
   if t1 = t then (* Regra 1: O tipo da expressão `e1` deve corresponder ao tipo declarado `t`. *)
     (* Regra 2: Verificamos o corpo `e2` em um *novo* ambiente temporário onde `x` é conhecido por ter o tipo `t`. *)
    let g' = (x,t)::g in (* `g'` é o novo ambiente com `(x,t)` adicionado na frente. *)
    typeinfer g' e2
   else raise (TypeError "Expressão associada a um id deve ter o tipo declarado")

  (* Para atribuição `e1 := e2` *)
 | Atrib(e1, e2) ->
   let t1 = typeinfer g e1 in
   let t2 = typeinfer g e2 in
   (match t1 with
    (* Regra 1: O lado esquerdo `e1` deve ser uma referência. *)
   | TyRef t -> if t = t2 then TyUnit else raise (TypeError "Tipo da expressão incompatível com o tipo da referência") (* Regra 2: O tipo de `e2` deve corresponder ao tipo interno `t` da referência. *)
   | _ -> raise (TypeError "Atribuição requer uma referência no lado esquerdo"))

  (* Para de-referenciação `!e1` *)
 | Deref(e1) ->
   let t1 = typeinfer g e1 in
   (match t1 with
   | TyRef t -> t (* O tipo de `!e1` é o tipo interno `t` da referência. *)
   | _ -> raise (TypeError "Derreferência (!) espera uma referência"))

  (* Para `new e1` *)
 | New(e1) -> TyRef (typeinfer g e1) (* O tipo de `new e1` é uma referência ao tipo de `e1`. *)

  (* Para uma sequência `e1; e2` *)
 | Seq(e1, e2) ->
   let t1 = typeinfer g e1 in
    (* A primeira parte de uma sequência deve ter tipo `unit` porque seu resultado é descartado. *)
   if t1 = TyUnit then typeinfer g e2 (* O tipo de toda a sequência é o tipo da segunda parte. *)
   else raise (TypeError "O lado esquerdo de uma sequência (;) deve ter tipo unit")

  (* Para um laço while *)
 | While(e1, e2) ->
   if typeinfer g e1 <> TyBool then raise (TypeError "A condição do while deve ser booleana");
   if typeinfer g e2 <> TyUnit then raise (TypeError "O corpo do while deve ter tipo unit");
   TyUnit (* A expressão `while` como um todo avalia para `unit`. *)

  (* Para um laço for *)
 | For(i, e_start, e_end, e_body) ->
   if typeinfer g e_start <> TyInt then raise (TypeError "Valor inicial do for deve ser int.");
   if typeinfer g e_end <> TyInt then raise (TypeError "Valor final do for deve ser int.");
    (* Verificamos o corpo do laço em um ambiente onde a variável de laço `i` é um inteiro. *)
   let g' = (i, TyInt) :: g in
   if typeinfer g' e_body <> TyUnit then raise (TypeError "Corpo do for deve ter tipo unit.");
   TyUnit (* A expressão `for` como um todo avalia para `unit`. *)

 | Read -> TyInt (* `read` sempre produz um inteiro. *)
 | Print(e1) -> (* `print` deve receber um inteiro e produz `unit`. *)
   if typeinfer g e1 = TyInt then TyUnit
   else raise (TypeError "Print espera uma expressão do tipo int")


(*
  ============================================================================
  PARTE 4: O AVALIADOR
  ---------------------
  Esta é a segunda fase principal. Após o código ser verificado, nós o executamos.
  Nosso avaliador usa "semântica operacional de passos pequenos". Isso significa
  que avaliamos expressões complexas um pequeno passo de cada vez. A função `step`
  realiza um passo, e a função `eval` a chama em um laço até terminar.
  ============================================================================
*)

(*
  PROPÓSITO:
  Uma função auxiliar que verifica se uma expressão é um "valor". Um valor é um
  resultado final que não pode ser mais simplificado (ex: `5`, `true`). Uma
  computação é uma expressão que *ainda não* é um valor (ex: `5 + 2`).
  A avaliação termina quando o programa inteiro se torna um único valor.
*)
let is_value (e:expr) : bool =
 match e with
 | Num _ | Bool _ | Unit | Loc _ -> true
 | _ -> false

(*
  PROPÓSITO:
  Substituição: `subs v x e` retorna uma nova expressão onde todas as ocorrências
  do nome de variável `x` na expressão `e` foram substituídas pelo valor `v`.
  É assim que damos significado a `let x = 5 in ... x ...`. Nós literalmente
  transformamos isso em `... 5 ...`.
*)
let rec subs (v:expr) (x:string) (e:expr) : expr =
 match e with
 | Id y -> if x=y then v else e
 | Binop(o,e1,e2) -> Binop(o, subs v x e1, subs v x e2)
 | If(e1,e2,e3) -> If(subs v x e1, subs v x e2, subs v x e3)
 | Let(y,t,e1,e2) ->
    (* Isso lida com o "sombreamento de variável". Se `let y = ...` usa o mesmo nome `x`
       que estamos substituindo, não devemos substituir dentro de seu corpo `e2`, pois
       esse `x` é uma variável diferente. *)
   if x=y then Let(y,t,subs v x e1, e2)
   else Let(y,t,subs v x e1, subs v x e2)
 | Atrib(e1,e2) -> Atrib(subs v x e1, subs v x e2)
 | Deref(e1) -> Deref(subs v x e1)
 | New(e1) -> New(subs v x e1)
 | Seq(e1,e2) -> Seq(subs v x e1, subs v x e2)
 | While(e1,e2) -> While(subs v x e1, subs v x e2)
 | Print(e1) -> Print(subs v x e1)
 | For(j, e1, e2, e3) -> if x=j then e else For(j, subs v x e1, subs v x e2, subs v x e3)
 | _ -> e (* Valores não contêm variáveis a serem substituídas. *)

(*
  PROPÓSITO:
  Um simples auxiliar que realiza o cálculo real (ex: adição, comparação)
  uma vez que ambos os operandos são valores.
*)
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

(*
  PROPÓSITO:
  Este é o coração do avaliador. Ele realiza exatamente UM pequeno passo de
  computação. Ele pega a expressão e o estado atuais e retorna a
  expressão e o estado após esse único passo.
  ex: `step( (2+3)*4, estado )` retornaria `( 5*4, estado )`.
*)
let rec step ((e, s): expr * state) : (expr * state) =
 match e with
  (* REGRA DE "DESSINTACTIZAÇÃO": Este é um truque poderoso. Em vez de escrever lógica complexa
     para o `for`, nós simplesmente o transformamos em uma combinação de `while` e `let`,
     que já sabemos como tratar. Isso mantém o interpretador mais simples. *)
 | For(i, e_start, e_end, e_body) ->
   let counter_id = "_counter_" ^ i in
   let end_id = "_end_" ^ i in
   let desugared =
    Let(counter_id, TyRef TyInt, New e_start,
     Let(end_id, TyInt, e_end,
      While(Binop(Leq, Deref (Id counter_id), Id end_id),
       Seq(
        Let(i, TyInt, Deref (Id counter_id), e_body),
        Atrib(Id counter_id, Binop(Sum, Deref (Id counter_id), Num 1))
       )
      )
     )
    )
   in (desugared, s) (* O resultado deste passo é a nova expressão, "dessintactizada". *)

  (* REGRAS ESTRUTURAIS: Estas regras definem a ordem de avaliação. Elas não
     fazem nenhum trabalho real, apenas decidem em qual subexpressão entrar
     para o próximo `step`.
     SINTAXE OCAML: `when <condição>` é uma "guarda" que adiciona uma condição ao padrão. *)

  (* Regra: Em `e1 + e2`, se `e1` não é um valor, execute o `step` em `e1` primeiro. *)
 | Binop(o, e1, e2) when not (is_value e1) ->
   let (e1', s') = step (e1, s) in (Binop(o, e1', e2), s')
  (* Regra: Em `v1 + e2`, se `e1` é um valor mas `e2` não, execute o `step` em `e2`. *)
 | Binop(o, v1, e2) when is_value v1 && not (is_value e2) ->
   let (e2', s') = step (e2, s) in (Binop(o, v1, e2'), s')
  (* Este mesmo padrão de "avaliar esquerda, depois avaliar direita" se aplica à maioria das construções. *)
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

  (* REGRAS COMPUTACIONAIS: Estas regras fazem o trabalho de verdade, uma vez que as subexpressões são valores. *)

 | Binop(o, v1, v2) when is_value v1 && is_value v2 -> (compute o v1 v2, s) (* Usa a função auxiliar para calcular o resultado. *)
 | If(Bool true, e2, e3) -> (e2, s)  (* Se a condição é `true`, damos o passo para o ramo `then`. *)
 | If(Bool false, e2, e3) -> (e3, s) (* Se é `false`, damos o passo para o ramo `else`. *)
 | Let(x, t, v1, e2) when is_value v1 -> (subs v1 x e2, s) (* Se a expressão do `let` é um valor, substitui-o no corpo. *)
 | Atrib(Loc l, v) when is_value v ->
    (* Regra: Atribuir o valor `v` à localização de memória `l`. *)
   let new_mem = (l, v) :: List.remove_assoc l s.mem in (* Atualiza a lista de memória. *)
    (* Retorna `Unit` e o novo estado. A sintaxe `{s with ...}` cria uma *cópia* do registro `s` com os campos especificados alterados. *)
   (Unit, {s with mem = new_mem})
 | Deref(Loc l) ->
    (* Regra: Ler o valor da localização de memória `l`. *)
   (match List.assoc_opt l s.mem with
   | Some v -> (v, s)
   | None -> raise (BugParser ("Endereço não encontrado na memória: " ^ (string_of_int l))))
 | New v when is_value v ->
    (* Regra: Alocar nova memória para o valor `v`. *)
   let l = s.next_loc in (* Pega o próximo endereço livre. *)
   let new_mem = (l, v) :: s.mem in (* Adiciona o novo par endereço-valor à memória. *)
   (Loc l, {s with mem = new_mem; next_loc = l + 1}) (* Retorna a nova localização, e atualiza o estado. *)
 | Seq(Unit, e2) -> (e2, s) (* Regra: Se a primeira parte de uma sequência terminou, prossiga para a segunda parte. *)
 | While(e1, e2) -> (If(e1, Seq(e2, While(e1, e2)), Unit), s) (* Transforma `while` em `if`. *)
 | Read ->
   (match s.input with
   | [] -> raise (BugParser "Entrada (input) vazia")
   | h::t -> (Num h, {s with input = t})) (* Pega um número do fluxo de entrada e atualiza o fluxo. *)
 | Print(Num n) ->
   (Unit, {s with output = s.output @ [n]}) (* Adiciona um número à lista de saída. `@` é a concatenação de listas. *)

 | _ -> raise NoRuleApplies (* Se ficarmos presos, é um bug. *)

(*
  PROPÓSITO:
  O laço principal de avaliação. É uma função recursiva simples que chama `step`
  repetidamente até que a expressão se torne um `valor` final.
*)
let rec eval ((e, s): expr * state) : (expr * state) =
 if is_value e then (e, s) (* Caso base: Se a expressão é um valor, terminamos! *)
 else (* Passo recursivo: *)
  try
   let (e', s') = step (e, s) in (* 1. Dê um pequeno passo. *)
   eval (e', s')                 (* 2. Chame `eval` novamente na nova expressão e estado. *)
  with ex -> raise ex (* Se `step` lançou um erro, apenas o propague para cima. *)


(*
  ============================================================================
  PARTE 5: FUNÇÕES DE ORQUESTRAÇÃO E UTILITÁRIOS
  ------------------------------------
  Este é o código base para executar nosso interpretador, imprimir os resultados
  de forma amigável e definir casos de teste.
  ============================================================================
*)

let rec strofvalue (v:expr) : string = (* Converte um valor final para uma string. *)
 match v with
 | Num n -> string_of_int n
 | Bool b -> string_of_bool b
 | Unit -> "()" (* `^` é o operador de concatenação de strings. *)
 | Loc l -> "loc<" ^ (string_of_int l) ^ ">"
 | _  -> raise (BugParser "Não é um valor")

let rec stroftipo (t:tipo) : string = (* Converte um tipo para uma string. *)
 match t with
 | TyInt -> "int"
 | TyBool -> "bool"
 | TyRef t1 -> "ref " ^ (stroftipo t1)
 | TyUnit -> "unit"

let print_output (out: int list) : unit = (* Imprime a lista de saída de forma legível. *)
 let rec p o =
  match o with
  | [] -> ()
  | h::t -> print_int h; print_string "; "; p t
 in
 print_string "["; p out; print_string "]"

(*
  A função principal de orquestração que une tudo.
  Ela recebe um programa, verifica seus tipos, o avalia e imprime os resultados.
*)
let inter (name: string) (e:expr) (i:int list) : unit =
 try (* `try...with` é o mecanismo de tratamento de exceções do OCaml. *)
  print_endline ("-- Executando teste: " ^ name ^ " --");
  print_string "Verificando tipos...\n";
  let t = typeinfer [] e in (* FASE 1: Executa o verificador de tipos. *)
  print_string ("Expressão bem tipada. Tipo inferido: " ^ (stroftipo t) ^ "\n");
  print_string "Avaliando...\n";
  let initial_state = { mem = []; input = i; output = []; next_loc = 0 } in
  let (v, final_state) = eval (e, initial_state) in (* FASE 2: Executa o avaliador. *)
  print_string "========================================\n";
  print_string ("Valor final: " ^ (strofvalue v) ^ " : " ^ (stroftipo t) ^ "\n");
  print_string "Saída (output): ";
  print_output final_state.output;
  print_string "\n========================================\n\n"
 with (* Este bloco captura quaisquer erros que foram lançados. *)
 | TypeError msg -> print_endline ("\nERRO DE TIPO: " ^ msg)
 | BugParser msg -> print_endline ("\nERRO DE EXECUÇÃO (BUG): " ^ msg)
 | DivZero -> print_endline ("\nERRO DE EXECUÇÃO: Divisão por zero.")
 | NoRuleApplies -> print_endline ("\nERRO DE EXECUÇÃO (BUG): Nenhuma regra de avaliação se aplica.")


(*
  ============================================================================
  PARTE 6: CASOS DE TESTE
  ------------------
  Aqui definimos programas em nossa linguagem simples (como dados do tipo `expr`)
  e os executamos.
  ============================================================================
*)

(* Um programa para calcular o fatorial. *)
let fat_test =
 Let("x", TyInt, Read,
  Let("z", TyRef TyInt, New (Id "x"),
   Let("y", TyRef TyInt, New (Num 1),
    Seq(
     While(Binop(Gt, Deref (Id "z"), Num 0),
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

(* Um programa para testar o laço `for`. *)
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

(*
  SINTAXE OCAML: `let () = ...`
  Esta é a maneira padrão de executar código quando um arquivo é executado. `()` é
  o valor "unit". Este é o ponto de entrada principal do nosso programa.
*)
let () =
 inter "Fatorial com while" fat_test [5];
 inter "Soma e contagem com for" for_test []

(* Uma suíte de testes menores e mais focados. *)
let arit_rel_test = Let("x", TyInt, Num 10, Let("y", TyInt, Num 5, Seq(Print(Binop(Sum, Id "x", Id "y")), Seq(Print(Binop(Sub, Id "x", Id "y")), Seq(Print(Binop(Mul, Id "x", Id "y")), Seq(Print(Binop(Div, Id "x", Id "y")), Seq(If(Binop(Lt, Id "x", Id "y"), Print(Num 1), Print(Num 0)), If(Binop(Leq, Id "x", Id "y"), Print(Num 1), Print(Num 0)))))))))
let if_test = Let("x", TyInt, Num 3, If(Binop(Lt, Id "x", Num 5), Print(Num 1), Print(Num 0)))
let ref_test = Let("x", TyRef TyInt, New (Num 10), Seq(Atrib(Id "x", Binop(Sum, Deref(Id "x"), Num 5)), Print(Deref(Id "x"))))
let seq_test = Seq(Print(Num 1), Seq(Print(Num 2), Print(Num 3)))
let while_test = Let("x", TyRef TyInt, New (Num 0), While(Binop(Lt, Deref(Id "x"), Num 3), Seq(Print(Deref(Id "x")), Atrib(Id "x", Binop(Sum, Deref(Id "x"), Num 1)))))
let for_test2 = Let("acc", TyRef TyInt, New (Num 0), Seq(For("i", Num 1, Num 3, Seq(Print(Id "i"), Atrib(Id "acc", Binop(Sum, Deref(Id "acc"), Id "i")))), Print(Deref(Id "acc"))))
let read_test = Let("a", TyInt, Read, Print(Id "a"))
let print_test = Print(Binop(Mul, Num 4, Num 2))
let logic_test = Seq(If(Binop(Eq, Num 3, Num 3), Print(Num 1), Print(Num 0)), Seq(If(Binop(Neq, Num 3, Num 4), Print(Num 1), Print(Num 0)), Seq(If(Binop(And, Bool true, Bool false), Print(Num 1), Print(Num 0)), If(Binop(Or, Bool true, Bool false), Print(Num 1), Print(Num 0)))))

(* Executando os testes menores. *)
let () =
 inter "1. Operações aritméticas e relacionais" arit_rel_test [];
 inter "2. If com booleano" if_test [];
 inter "3. Referências, atribuição e derreferência" ref_test [];
 inter "4. Sequência com unit" seq_test [];
 inter "5. Laço while simples" while_test [];
 inter "6. Laço for de 1 a 3 com soma" for_test2 [];
 inter "7. Leitura com read" read_test [42];
 inter "8. Print com expressão" print_test [];
 inter "9. Operadores lógicos e igualdade" logic_test []
