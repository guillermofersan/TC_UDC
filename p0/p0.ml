(*Ejercicio 1: funcion mapdoble*)

let rec mapdoble f1 f2 = function 
    []-> []
  | h::[] -> [f1 h]
  | h::t -> f1(h)::mapdoble f2 f1 t;;
(*
tipo: ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>

Valor de:
mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;
Error: This expression has type string but an expression was expected of type int
Se produce un error debido a que se espera que la lista devuelta sea de un solo tipo 'b

Indica el tipo de:
let y = function x -> 5 in mapdoble y;;

- : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>
*)

(*----------------------------------------------------------------------------------------------*)
(*Ejercicio 2: primero_que_cumple*)

let rec primero_que_cumple f = function 
    []-> raise Not_found
  | h::t -> if f(h) then h else primero_que_cumple f t;;

  (*tipo: val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun>*)

let existe f l = 
  try (let _ = primero_que_cumple f l in true) with Not_found -> false;;

  (*tipo: val existe : ('a -> bool) -> 'a list -> bool = <fun>*)

let asociado l key = 
  snd(primero_que_cumple (function (a,b) -> a = key) l);;  

  (*tipo: val asociado : ('a * 'b) list -> 'a -> 'b = <fun>*)

(*----------------------------------------------------------------------------------------------*)
(*Ejercicio 3: arbol binario*)

type 'a arbol_binario =
  Vacio
  | Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

(*let t = Nodo(3,Nodo(2,Vacio,Vacio),Nodo(5,Nodo(4,Vacio,Vacio),Nodo(1,Vacio,Vacio)));;*)


let rec in_orden = function
    Vacio -> []
  | Nodo (x, ni, nd) -> (in_orden ni) @ [x] @ (in_orden nd);;
  (*val in_orden : 'a arbol_binario -> 'a list = <fun>*)


let rec pre_orden = function
    Vacio -> []
  | Nodo (x, ni, nd) -> x::((pre_orden ni) @ (pre_orden nd));;
  (*val pre_orden : 'a arbol_binario -> 'a list = <fun>*)


let rec post_orden = function 
    Vacio -> []
  | Nodo (x, ni, nd) -> (post_orden ni) @ (post_orden nd) @ [x];;
  (*val post_orden : 'a arbol_binario -> 'a list = <fun>*)

(*
let rec anchura t =  
  let rec aux q = function
    [] -> l
  | Nodo(x,Vacio)   
*)


(*----------------------------------------------------------------------------------------------*)
(*Ejercicio 4: conjunto*)

type 'a conjunto = Conjunto of 'a list;;

(*
Por ejemplo, el conjunto vacío se podría representar mediante el siguiente valor:
let conjunto_vacio = Conjunto [];;
let c = Conjunto([0;1;2;3;4]);;
*)

let rec pertenece x = function
    Conjunto [] -> false
  | Conjunto (h::t) -> if h=x then true else pertenece x (Conjunto t);;
  (*val pertenece : 'a -> 'a conjunto -> bool = <fun>*)

let rec agregar x c =
  if pertenece x c then c else match c with
  Conjunto l -> (Conjunto (l @ [x]));;
  (*val agregar : 'a -> 'a conjunto -> 'a conjunto = <fun>*)

let rec conjunto_of_list l =
  let rec aux (Conjunto l2) = function 
    [] -> (Conjunto l2)
    | h::t -> aux (agregar h (Conjunto l2)) t
  in aux (Conjunto []) l;;
  (*val conjunto_of_list : 'a list -> 'a conjunto = <fun>*)

let rec suprimir x (Conjunto l) = 
  let rec aux x = function 
      [] -> []
    | h::t -> if h=x then t else h::(aux x t)
  in Conjunto (aux x l);;
  (*val suprimir : 'a -> 'a conjunto -> 'a conjunto = <fun>*)

let rec cardinal (Conjunto l) =
  let rec aux count = function 
      [] -> count 
    | _::t -> aux (count+1) t
  in aux 0 l;;
  (*val cardinal : 'a conjunto -> int = <fun>*)

let union c1 (Conjunto l2) =
  let rec aux (Conjunto l1) = function 
      [] -> l1
    | h::t -> if pertenece h c1 then (aux c1 t) else h::(aux (Conjunto l1) t) 
  in aux c1 l2;;  
  (*val union : 'a conjunto -> 'a conjunto -> 'a list = <fun>*)













  
let list_of_conjunto (Conjunto l) = l;
