
type bop = Sum | Sub | Mul | Div 
         | Eq | Gt | Lt | Neq
         | And | Or

          
type tipo = 
    TyInt 
  | TyBool
  | TyFn of tipo * tipo 
            

type expr = 
    Num of int
  | Bool of bool 
  | Binop of bop * expr * expr 
  | If  of expr * expr * expr 
  | Id  of string 
  | Fn of string * tipo * expr
  | Let of string * tipo *  expr * expr
  | App of expr * expr 
  | LetRec of string * tipo * expr * expr
           

  
(*=========== TYPEINFER ===================*)           
type tyEnv =  (string * tipo) list
    
    
type value = 
  | VNum of int
  | VBool of bool
  | VClos of string * expr * runEnv
  | VRecClos of string * string * expr * runEnv
                  
and runEnv =  (string * value) list
    
let rec lookup env x = 
  match env with 
    [] -> None
  | (y,t):: tail -> if x=y then Some t else lookup tail x
          
          
           
exception TypeError of string
    
let msg_aritmeticos = "operandos de ops aritméticas devem ser inteiros" 
let msg_relacionais = "operandos de ops relacionais devem ser inteiros"
let msg_booleanos = "operandos de ops booleanas devem ser booleanos" 
let msg_thenelse = "expressões de thene de else devem ser domesmo tipo " 
let msg_condif = "condição de if-then-else deve ser booleana" 
let msg_idndeclarado = "identificador não declarado"
let msg_letconflito = "expressão associada a um id deve ter o tipo declarado" 
let msg_tipoarg = "argumento deve ser do tipo esperado pela função "
let msg_funesperada = "lado esquerdo de aplicação deve ser do tipo função" 
let msg_letrec =    "funcao recursiva deve se do tipo declarado"
  
exception BugParser 
  
let rec typeinfer (g:tyEnv) (e:expr) : tipo = 
  match e with 
    Num _ -> TyInt
  | Bool _ -> TyBool 
    
  | Binop(o,e1,e2) -> 
      let t1 = typeinfer g e1  in
      let t2 = typeinfer g e2  in
      (match o with
         Sum | Sub | Mul | Div ->
           if (t1=TyInt) && (t2=TyInt) then TyInt else raise (TypeError msg_aritmeticos)
       | Eq | Gt | Lt | Neq ->
           if (t1=TyInt) && (t2=TyInt) then TyBool else raise (TypeError msg_relacionais)
       | And | Or -> 
           if (t1=TyBool) && (t2=TyBool) then TyBool else raise (TypeError msg_booleanos))
      
       
  | If(e1,e2,e3) ->  
      let t1 = typeinfer g e1  in
      if (t1=TyBool) then
        let t2 = typeinfer g e2  in
        let t3 = typeinfer g e3  in
        if (t2=t3) then t2 else raise (TypeError msg_thenelse)
      else raise (TypeError msg_condif)
                      
  | Id x ->
      (match lookup g x with
         None -> raise (TypeError (msg_idndeclarado ^ " - " ^ x))
       | Some t -> t)
      
  | Fn(x,t,e1) -> 
      let g' = (x,t)::g in 
      let t1 = typeinfer g' e1 in 
      TyFn(t,t1)
                     
  | Let(x,t,e1,e2) -> 
      let t1 = typeinfer g e1 in 
      let g' = (x,t)::g in
      let t2 = typeinfer g' e2 in
      if t1=t then t2 else raise (TypeError msg_letconflito)
      
                        
  | App(e1,e2) -> 
      let t1 = typeinfer g e1  in
      (match t1 with
         TyFn(t,t') -> 
           let t2 = typeinfer g e2  in
           if t=t2 then t' else raise (TypeError msg_tipoarg)
       | _ -> raise (TypeError msg_funesperada)) 
  
  | LetRec(f,TyFn(tin,tout), Fn(x,tx,e1), e2) -> 
      let g_com_tf = (f,TyFn(tin,tout)) :: g in
      let g_com_tf_tx = (x,tx) :: g_com_tf in
      if (typeinfer g_com_tf_tx e1) = tout 
      then typeinfer g_com_tf e2
      else raise (TypeError msg_letrec)
      
  | _ -> raise BugParser 
      

(* ======= AVALIADOR =========================*)

exception DivZero 
exception NoRuleApplies
  
let rec compute (o:bop) (v1: value) (v2:value) : value = 
  match (v1,v2) with
    (VNum n1, VNum n2) -> 
      (match o with 
         Sum -> VNum (n1+n2)
       | Sub -> VNum (n1-n2)
       | Mul -> VNum (n1*n2)
       | Div -> if n2 != 0 then VNum (n1/n2) else raise DivZero
       | Eq -> VBool(n1==n2)
       | Gt -> VBool(n1>n2)
       | Lt -> VBool(n1<n2)
       | Neq -> VBool(n1!=n2)
       | _   -> raise NoRuleApplies)
  | (VBool b1, VBool b2) ->
      (match o with 
         And -> VBool(b1 && b2)
       | Or  -> VBool(b1 || b2)
       | _   -> raise NoRuleApplies)
  | _ -> raise NoRuleApplies 
           


exception BugTypeInfer 
  
let rec eval (env: runEnv) (e:expr) : value = 
  match e with 
  
    Num n -> VNum n
               
  | Bool b ->  VBool b
    
  | Binop(op,e1,e2) ->  
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      compute op v1 v2
       
  | If(e1,e2,e3) ->   
      (match eval env e1 with
       | VBool true -> eval env e2
       | VBool false -> eval env e3
       | _  -> raise BugTypeInfer)
                      
  | Id x -> 
      (match lookup env x with
         None -> raise (BugTypeInfer)
       | Some v -> v)
  
  | Fn(x,t,e1) ->  VClos(x,e1,env)
  
  | Let(x,t,e1,e2) ->  
      let v1 = eval env e1 in 
      eval ((x,v1) :: env) e2
  
  | App(e1,e2) ->  
      let v2 = eval env e2 in
      (match eval env e1  with 
       | VClos      (x,e,env') -> eval ((x,v2) :: env') e
       | VRecClos (f,x,e,env') -> eval ((f,VRecClos(f,x,e,env'))::(x,v2) :: env') e 
       | _ -> raise BugTypeInfer)

  
  | LetRec(f,TyFn(tin,tout), Fn(x,tx,e1), e2) ->  
      eval ((f, VRecClos(f,x,e1,env)) :: env)   e2
      
  | _ -> raise BugParser
      

 

           

                       

(*===== INTEPRETADOR ========================*)

exception NotValue
  
let rec strofvalue (v:value) : string = 
  match v with 
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClos _ ->  "fun"
  | VRecClos _ -> "fun"
    
let rec stroftipo (t:tipo) : string = 
  match t with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFn(t1,t2) -> (stroftipo t1) ^ "-->" ^ (stroftipo t2)
    
    
  
let rec inter (e:expr) : unit = 
  try
    let t = typeinfer []  e in
    let v = eval [] e in
    print_endline ("= " ^ (strofvalue v) ^  ":" ^ (stroftipo t) )
  with 
    TypeError msg -> print_endline ("Erro de tipo: " ^ msg)
  

(*======== TESTES ===================*) 

(* 
let a:int = 10 in
let b:int = 2 in 
a / b
*)

let tst1 = Let("a", TyInt, Num 10, 
               Let ("b", TyInt, Num 2, 
                    Binop (Mul, Id "a", Id "b")))
(*

let dobro : int -> int = fn x:int => x * 2 in
dobro 10
*)   
  
let tst2 = Let("dobro", TyFn(TyInt,TyInt),Fn("x",TyInt,Binop (Mul, Id "x", Num 2)) , 
               App(Id "dobro", Num 10))
    
    (*

let dobro : int -> int = fn x:int => x * 2 in
dobro 
*)   
  
let tst3 = Let("dobro", TyFn(TyInt,TyInt),Fn("x",TyInt,Binop (Mul, Id "x", Num 2)) , 
               Id "dobro")
 
    (* teste para subs  *)

    (*  fn x:int => x + z  *)
let tst4 = Fn("x",TyInt,  Binop(Sum, Id "x", Id "z"))
    
    (* let x = x + 10 in 2 * x *)
let tst5 = Let("x", TyInt, Binop(Sum, Id "x", Num 10), 
               Binop(Mul, Num 2, Id "x"))
    
    
(*   com acúcar sintático:
   -- alcula x elevado ao expoente i  --- assume y >=0 
   let rec pow (x:int) (y:int) : int = 
          if y = 0 then 1 else x * (pow x (y-1))  
   in (pow 3) 4 
     
sem açucar sintático:

  let rec pow: int -> (int -> int) = 
    fn x:int => fn y:int => if y = 0 then 1 else x * (pow x (y-1))  
  in (pow 3) 4 

*)          

let ys1 = Binop(Sub, Id "y", Num 1)     (* y - 1  *)
  
let powapp  =   App(App(Id "pow", Id "x"), ys1)   (* pow *)
                                           
let xp =   Binop(Mul, Id "x", powapp)
      
    (* fn y:int => if y=0 then 1 else x * (pow x (y-1))    *) 
let ebdy = Fn("y",TyInt,If(Binop(Eq, Id "y", Num 0) , Num 1, xp))  
  
let tst6 = 
  LetRec("pow", TyFn(TyInt, TyFn(TyInt,TyInt)), (* pow: int -> int -> int*)
         Fn("x", TyInt, ebdy), (* fn x: int => ebdy  *)
         App(App(Id "pow", Num 3), Num 4)) (* in  (pow 3) 4    *)
                                            
  
    
let testes = [tst1;tst2;tst3;tst4;tst5;tst6]
             
let runtests =  List.map inter testes
    