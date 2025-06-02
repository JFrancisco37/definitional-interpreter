(* Linguagem de tipos *)

type tipo = 
    TyInt 
  | TyBool
  | TyFn   of tipo * tipo
  | TyPair of tipo * tipo
  | TyList of tipo          
  | TyVar  of int   (* variáveis de tipo -- números *) 


(* free type variables em um tipo *)
            
let rec ftv (tp:tipo) : int list =
  match tp with
    TyInt         -> []
  | TyBool        -> []
  | TyFn(t1,t2)   -> (ftv t1) @ (ftv t2)
  | TyPair(t1,t2) -> (ftv t1) @ (ftv t2)
  | TyList(t1)    -> (ftv t1)
  | TyVar(n)      -> [n] 


                   
(* impressao legível de tipos  *)

let rec tipo_str (tp:tipo) : string =
  match tp with
    TyInt           -> "int"
  |  TyBool          -> "bool"       
  |  TyFn   (t1,t2)  -> "("  ^ (tipo_str t1) ^
                        "->" ^ (tipo_str t2) ^ ")"
  |  TyPair (t1,t2)  -> "("  ^ (tipo_str t1) ^
                        "*" ^ (tipo_str t2) ^ ")"
                      
  |  TyVar  x        -> (string_of_int x)
  |  TyList t1       -> "(" ^ (tipo_str t1) ^ " list)"

            
type ident = string
  
type op = Sum | Sub | Mult | Eq | Gt | Lt | Geq | Leq


(* Linguagem de termos *)
                                                
type expr  = 
    Num of int  
  | Var of ident 
  | True
  | False
  | Binop of op * expr * expr
  | Pair of expr * expr 
  | Fst of expr
  | Snd of expr 
  | If of expr * expr * expr
  | Fn of ident * expr                    (* sem anotação *)
  | App of expr * expr
  | Let of ident * expr * expr            (* sem anotação *)
  | LetRec of ident * ident * expr * expr (* sem anotação *)
  | Nil                                   (* sem anotação *)
  | Cons of expr * expr
  | Head of expr
  | Tail of expr
  | IsEmpty of expr        
  | Raise
  | Try of expr * expr



(* impressão legível de termo *)
     
let rec termo_str (e:expr) : string  =
  match e with
    Var x -> x            
  | Num n -> string_of_int n;
  | True  -> "true"
  | False -> "false"
  | Binop (o,e1,e2) ->  
      let s = (match o with
            Sum  -> "+"
          | Sub  -> "-"
          | Mult -> "*"
          | Leq  -> "<="
          | Geq  -> ">="
          | Eq   -> "="
          | Lt   -> "<"
          | Gt   -> ">") in
      "( " ^ (termo_str e1) ^ " " ^ s ^ " " ^ (termo_str e2) ^ " )"
  | Pair (e1,e2) ->  "(" ^ (termo_str e1) ^ "," ^ (termo_str e2) ^ ")"  
  | Fst e1 -> "fst " ^ (termo_str e1) 
  | Snd e1 -> "snd " ^ (termo_str e1) 
  | If (e1,e2,e3) -> "(if " ^ (termo_str e1) ^ " then "
                     ^ (termo_str e2) ^ " else "
                     ^ (termo_str e3) ^ " )" 
  | Fn (x,e1) -> "(fn " ^ x ^ " => " ^ (termo_str e1) ^ " )"
  | App (e1,e2) -> "(" ^ (termo_str e1) ^ " " ^ (termo_str e2) ^ ")"
  | Let (x,e1,e2) -> "(let " ^ x ^ "=" ^ (termo_str e1) ^ "\nin "
                     ^ (termo_str e2) ^ " )"
  | LetRec (f,x,e1,e2) -> "(let rec " ^ f ^ "= fn " ^ x ^ " => "
                          ^ (termo_str e1) ^ "\nin " ^ (termo_str e2) ^ " )"
  | Nil -> "[]"
  | Cons (e1,e2) -> (termo_str e1) ^ "::" ^ (termo_str e2)
  | Head e1 -> "hd " ^ (termo_str e1)
  | Tail e1 -> "tl " ^ (termo_str e1)
  | IsEmpty e1 -> "isempty " ^ (termo_str e1)           
  | Raise   -> "raise"
  | Try (e1,e2) -> "try " ^ (termo_str e1) ^ " with " ^ (termo_str e2)







         
(* ambientes - modificados para polimorfismo *)
            
type amb = (ident * (int list) * tipo) list 
    
let empty_amb : amb = [] 
    
let rec lookup amb x : (int list * tipo) option = 
  match amb with
    [] -> None
  | (y,v,t) :: tl -> if (y=x) then Some (v,t) else lookup tl x
                   
let rec update (amb: amb) (x:ident) (v:int list) (t:tipo) : amb = 
  (x,v,t) :: amb


(* remove elementos identicos *)
let nub l =
  let ucons h t = if List.mem h t then t else h::t in
  List.fold_right ucons l []
 

(* calcula l1 - l2 (como sets) *)
let diff (l1:'a list) (l2:'a list) : 'a list =
  List.filter (fun a -> not (List.mem a l2)) l1

  
(* calcula todas as variáveis de tipo livres 
   do ambiente *)           
let rec ftv_amb' (g:amb) : int list =
  match g with
    []           -> []
  | (x,v,tp)::t  -> (diff (ftv tp) v)
                    @(ftv_amb' t)
let ftv_amb (g:amb) : int list = nub (ftv_amb' g)



  
               
(* restrições de tipo  *)
  
type constraints = (tipo * tipo) list 

(* 
   a lista 
       [ (t1,t2) ; (u1,u2) ] 
   representa o conjunto de restrições 
       { t1=t2, u1=u2 } 
 *)
                 

(* imprime restrições *)

let rec print_constr (c:constraints) =
  match c with
    []       -> print_string "\n";
  | (a,b)::t -> print_string (tipo_str a);
      print_string " = ";
      print_string (tipo_str b);
      print_string "\n";
      print_constr t

                 

(* código para geração de novas variáveis de tipo *)
                 
let lastvar = ref 0

let newvar (u:unit) : int =
  let x = !lastvar
  in lastvar := (x+1) ; x




(* substituições *)
                      
type subst = (int*tipo) list


(* consulta a substituição *)
           
let rec lookupsubs amb x : (tipo) option = 
  match amb with
    [] -> None
  | (y,t) :: tl -> if (y=x) then Some (t) else lookupsubs tl x


(* imprime substituições  *)
                
let rec print_subst (s:subst) =
  match s with
    []       -> print_string "\n";
  | (a,b)::t -> print_int a;
      print_string " |-> ";
      print_string (tipo_str b);
      print_string "\n";
      print_subst t


           
(* aplicação de substituição a tipo *)
           
let rec appsubs (s:subst) (tp:tipo) : tipo =
  match tp with
    TyInt           -> TyInt
  |  TyBool          -> TyBool       
  |  TyFn   (t1,t2)  -> TyFn   (appsubs s t1, appsubs s t2)
  |  TyPair (t1,t2)  -> TyPair (appsubs s t1, appsubs s t2)
  |  TyVar  x        -> (match lookupsubs s x with
        None        -> TyVar x
      | Some tp'    -> tp')
  |  TyList t1       -> TyList (appsubs s t1)                      


                      
(* composição de substituições: s1 o s2  *)
let rec compose (s1:subst) (s2:subst) : subst =
  let r1 = List.map (fun (x,y) -> (x, appsubs s1 y))      s2 in
  let (vs,_) = List.split s2                                 in
  let r2 = List.filter (fun (x,y) -> not (List.mem x vs)) s1 in
  r1@r2


(* aplicação de substituição a coleção de constraints *)
let rec appconstr (s:subst) (c:constraints) : constraints =
  List.map (fun (a,b) -> (appsubs s a,appsubs s b)) c

  
(* testa se variável ocorre em tipo *)
                 
let rec var_in_tipo (v:int) (tp:tipo) : bool =
  match tp with
    TyInt           -> false
  |  TyBool          -> false       
  |  TyFn   (t1,t2)  -> (var_in_tipo v t1)||(var_in_tipo v t2)
  |  TyPair (t1,t2)  -> (var_in_tipo v t1)||(var_in_tipo v t2)
  |  TyVar  x        -> v==x
  |  TyList t1       -> var_in_tipo v t1                      




(* cria novas variáveis para politipos durante a consulta dos mesmos *) 
                       
let rec renamevars (v:int list) (tp:tipo) : tipo =
  match v with
    []      -> tp
  | (h::v') -> let h' = newvar() in
      appsubs [(h,TyVar h')] (renamevars v' tp)

  
(* coleta de restrições e unificação definidas via recursão mútua *)
              
exception UnifyFail of (tipo*tipo)

exception CollectFail of string

               
let rec collect (g:amb) (e:expr) : (constraints*tipo)  =

  match e with

 (* caso alterado para polimorfismo:
    ambientes registram variáveis de tipo
    quantificadas, que recebem nomes
    novos a cada consulta ao ambiente. 
  *)
   
    Var x   ->
      (match lookup g x with
         None        -> raise (CollectFail x)
       | Some (v,tp) -> ([],renamevars v tp))

  | Num n -> ([],TyInt)

  | True  -> ([],TyBool)

  | False -> ([],TyBool)

  | Binop (o,e1,e2) ->  
      if List.mem o [Sum;Sub;Mult]
      then
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyInt)
      else   
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyBool)    

  | Pair (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2, TyPair(tp1,tp2))    
        
  | Fst e1 ->
      let tA = newvar() in
      let tB = newvar() in
      let (c1,tp1) = collect g e1 in
      (c1@[(tp1,TyPair(TyVar tA, TyVar tB))], TyVar tA)

  | Snd e1 ->
      let tA = newvar() in
      let tB = newvar() in
      let (c1,tp1) = collect g e1 in         
      (c1@[(tp1,TyPair(TyVar tA,TyVar tB))], TyVar tB)         

  | If (e1,e2,e3) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      let (c3,tp3) = collect g e3 in         
      (c1@c2@c3@[(tp1,TyBool);(tp2,tp3)], tp2)

  | Fn (x,e1) ->
      let tA       = newvar()           in
      let g'       = (x,[],TyVar tA)::g in
      let (c1,tp1) = collect g' e1      in
      (c1, TyFn(TyVar tA,tp1))
         
  | App (e1,e2) -> 
      let (c1,tp1) = collect  g e1 in
      let (c2,tp2) = collect  g e2  in
      let tA       = newvar()       in
      let tB       = newvar()       in
      (c1@c2@[(tp1,TyFn(TyVar tA,TyVar tB));(tp2,TyVar tA)]
      , TyVar tB) 

  (* let alterado com suporte a polimorfismo Hindley-Milner 

  Versão monomórfica (para comparação):       

  | Let (x,e1,e2) ->
         let (c1,tp1) = collect  g e1   in
         let g'       = (x,tp1)::g      in
         let (c2,tp2) = collect  g' e2  in
         (c1@c2, tp2)  
*)

        
  | Let (x,e1,e2) ->
      let (c1,tp1) = collect  g e1                    in
     
      let s1       = unify c1                         in
      let tf1      = appsubs s1 tp1                   in
      let polivars = nub (diff (ftv tf1) (ftv_amb g)) in
      let g'       = (x,polivars,tf1)::g              in

      let (c2,tp2) = collect  g' e2                   in                     
      (c1@c2, tp2)

  (* LetRec alterado com suporte a polimorfismo Hindley-Milner 
                     
    Versão monomórfica para comparação:
         
  | LetRec (f,x,e1,e2) ->
     let tA = newvar() in
     let tB = newvar() in
     let g2 = (f,TyFn(TyVar tA,TyVar tB))::g in
     let g1 = (x,TyVar tA)::g2               in
     let (c1,tp1) = collect g1 e1            in
     let (c2,tp2) = collect g2 e2            in
     (c1@[(tp1,TyVar tB)]@c2, tp2)
*)     

  | LetRec (f,x,e1,e2) ->
      let tA = newvar() in
      let tB = newvar() in   
      let g2 = (f,[],TyFn(TyVar tA,TyVar tB))::g            in
      let g1 = (x,[],TyVar tA)::g2                          in
      let (c1,tp1) = collect g1 e1                          in

      let s1       = unify (c1@[(tp1,TyVar tB)])            in
      let tf1      = appsubs s1 (TyFn(TyVar tA,TyVar tB))   in
      let polivars = nub (diff (ftv tf1) (ftv_amb g))       in
      let g'       = (f,polivars,tf1)::g                    in     

      let (c2,tp2) = collect g' e2                          in
      (c1@[(tp1,TyVar tB)]@c2, tp2)


  | Nil ->
      let tA = newvar() in
      ([], TyList (TyVar tA))
     
  | Cons (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp2,TyList tp1)], tp2)
             
  | Head e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], TyVar tA)

  | IsEmpty e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], TyBool)

  | Tail e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], tp1)

  | Raise ->
      let tA = newvar() in
      ([],TyVar tA)
          
  | Try (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp1,tp2)],tp1)      

and  unify (c:constraints) : subst =
  match c with
    []                                   -> []
  | (TyInt,TyInt)::c'                    -> unify c'
  | (TyBool,TyBool)::c'                  -> unify c'
  | (TyFn(x1,y1),TyFn(x2,y2))::c'        -> unify ((x1,x2)::(y1,y2)::c')
  | (TyPair(x1,y1),TyPair(x2,y2))::c'    -> unify ((x1,x2)::(y1,y2)::c')
  | (TyList x1,TyList x2)::c'            -> unify ((x1,x2)::c')
  | (TyVar x1, TyVar x2)::c' when x1==x2 -> unify c'

  | (TyVar x1, tp2)::c'                  -> if var_in_tipo x1 tp2
      then raise (UnifyFail(TyVar x1, tp2))
      else compose
          (unify (appconstr [(x1,tp2)] c'))
          [(x1,tp2)]  

  | (tp1,TyVar x2)::c'                  -> if var_in_tipo x2 tp1
      then raise (UnifyFail(tp1,TyVar x2))
      else compose
          (unify (appconstr [(x2,tp1)] c'))
          [(x2,tp1)] 

  | (tp1,tp2)::c' -> raise (UnifyFail(tp1,tp2))




                   

(* INFERÊNCIA DE TIPOS - CHAMADA PRINCIPAL *)
       
let typeinfer (e:expr) : unit =
  print_string "\nTermo:\n";
  print_string (termo_str e);
  print_string "\n\n";
  try
    let (c,tp) = collect [] e  in
    let s      = unify c       in
    let tf     = appsubs s tp  in
    print_string "\nRestrições:\n";
    print_constr c;
    print_string "Tipo inferido: ";    
    print_string (tipo_str tp);
    print_string "\n\nSubstituição:\n";
    print_subst s;
    print_string "Tipo inferido (após subs): ";
    print_string (tipo_str tf);
    print_string "\n\n"

  with
    
  | CollectFail x   ->
      print_string "Erro: variável ";
      print_string x;
      print_string "não declarada!\n\n"
                     
  | UnifyFail (tp1,tp2) ->
      print_string "Erro: impossível unificar os tipos\n* ";
      print_string (tipo_str tp1);
      print_string "\n* ";
      print_string (tipo_str tp2);
      print_string "\n\n";
      lastvar := 0
                           

                   
(*********   EXEMPLOS DE CÓDIGO  ***********)
            
                                                  
(*   exemplo de código 1

   let rec pow = 
        fn x => 
          fn y => 
            if y = 0 
              then 1 
              else x * (pow x (y-1))  
   in 
       (pow 3) 4 


   deve ter tipo "int"

*)

                

(*   (y-1)              *)     
let ys1 = Binop(Sub, Var "y", Num 1)  

(*   pow x (y-1)        *)        
let powapp  =   App(App(Var "pow", Var "x"), ys1)   

(*   x*(pow x (y-1))    *)           
let xp =   Binop(Mult, Var "x", powapp)
   

(*   if y=0 then 1 else x*(pow x (y-1))    *)
let ebdy =  Fn("y", If(Binop(Eq, Var "y", Num 0) , Num 1, xp)) 


(*  let rec pow = fn x => ... in (pow 3) 4 *)
                          
let ex1 = 
  LetRec("pow", "x", ebdy, App(App(Var "pow", Num 3),Num 4)) 



(*   exemplo de código 2

  if raise 
    then (cons 1 empty)
    else empty

  deve ter tipo "int list"

*)

let ex2 = If(Raise, Cons(Num 1,Nil), Nil)

  
(*   exemplo de código 3

  if tail raise 
    then (cons true empty)
    else raise

  deve falhar pois "tail" sempre retorna um tipo de lista

*)

let ex3 = If(Tail(Raise), Cons(Num 1,Nil), Nil)


(* exemplo de código 4


   let id = fn x => x
   in if (id true) then (id 0) else 1

 deve retornar "int" em um sistema
 polimórfico, mas falharia em um sistema
 monomórfico.

*)
        

let ex4 = Let("id",Fn("x",Var "x"),
              If(App(Var "id",True), App(Var "id",Num 0), Num 1))

        
(*   exemplo de código 5

  let double = fn f => fn x => f (f x) 
  in  if true
        then double (fn y => y) 3
        else double (fn y => false) true
 
  deve falhar pois double não consegue unificar simultaneamente
  com (int->int)->int->int e (bool->bool)->bool->bool

*)

let doublefn = Fn("f",Fn("x",App(Var "f",App(Var "f",Var "x"))))

let doubleif = If(
    True,
    App(App(Var "double",Fn("y",Var "y")),Num 3),
    App(App(Var "double",Fn("y",False)),True)
  )
        
let ex5 = Let("double",doublefn,doubleif)



(* exemplo de código 6

   let id = fn x => x
   in if (id true) then id else id

em um sistema let-polimórfico deve retornar o 
tipo X -> X 

em um sistema monomórfico retornaria
bool -> bool

*)

let ex6 = Let("id",Fn("x",Var "x"),
              If(App(Var "id",True), Var "id", Var "id"));;





(* exemplo de código 7

   let rec f = fn y => (fn z=>z) y
   in f

em um sistema let-polimórfico deve retornar o 
tipo X -> X

*)

let ex7 = LetRec("f","y",
                 App(Fn("z",Var "z"),Var "y"),
                 Var "f");;


(* exemplo de código 8

   let rec f = fn y => (f y)
   in f 

em um sistema polimórfico deve retornar o 
tipo  X -> Y

*)

let ex8 = LetRec("f","y",
                 App(Var "f",Var "y"),
                 Var "f");;


(* exemplo de código 9

   let rec f = fn y => (fn z => f z) 2
   in f 0

em um sistema polimórfico deve retornar o 
tipo X 

*)

let ex9 = LetRec("f","y",
                 App(Fn("z",App(Var "f",Var "z")),Num 2),
                 App(Var "f",Num 0));;


(* exemplo de código 10

let rec map = fn f => (fn l => if isempty l
                       then nil
                       else cons (f (head l)) (map f (tail l)))
                                        
in map

  em um sistema polimórfico deve retornar o 
  tipo (A->B) -> A list -> B listf

*)



let ex10 = LetRec ("map","f",
                   Fn("l", If(IsEmpty(Var "l"),
                              Nil,
                              Cons (App(Var "f",Head(Var "l")),
                                    App(App(Var "map",Var "f"),Tail(Var "l"))))),
                   Var "map");;


let incr = Fn("x",Binop(Sum, Var "x", Num 1))
    
let incrsnd = Fn("x", Binop(Sum, Snd (Var "x"), Num 1))

let ex11 = LetRec ("map","f",
                   Fn("l", If(IsEmpty(Var "l"),
                              Nil,
                              Cons (App(Var "f",Head(Var "l")),
                                    App(App(Var "map",Var "f"),Tail(Var "l"))))),
                   Pair(App(App(Var "map", incr),Cons(Num 1, Cons(Num 3, Nil))), 
                        App(App(Var "map", incrsnd),
                            Cons(Pair(Num 0,Num 10), Cons(Pair(Num 0,Num 30), Nil)))))
