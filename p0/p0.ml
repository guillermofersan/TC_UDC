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

(*
let t = Nodo(3,Nodo(2,Vacio,Vacio),Nodo(5,Nodo(4,Vacio,Vacio),Nodo(1,Vacio,Vacio)));;
let t2 = Nodo(3,Nodo(2,Nodo(9,Nodo(0,Vacio,Vacio),Vacio),Vacio),Nodo(5,Nodo(4,Vacio,Vacio),Nodo(1,Vacio,Vacio)));;

*)


let rec in_orden = function
    Vacio -> []
  | Nodo (x, ni, nd) -> (in_orden ni) @ (x::(in_orden nd));;
  (*val in_orden : 'a arbol_binario -> 'a list = <fun>*)


let rec pre_orden = function
    Vacio -> []
  | Nodo (x, ni, nd) -> x::((pre_orden ni) @ (pre_orden nd));;
  (*val pre_orden : 'a arbol_binario -> 'a list = <fun>*)


let rec post_orden = function 
    Vacio -> []
  | Nodo (x, ni, nd) -> (post_orden ni) @ (post_orden nd) @ [x];;
  (*val post_orden : 'a arbol_binario -> 'a list = <fun>*)

let anchura tree =
    let rec aux l queue = match queue with
        [] -> List.rev l
      | Vacio::t -> aux l t
      | Nodo (x, Vacio, Vacio)::t -> aux (x::l) t 
      | Nodo (x, ni, nd)::t -> aux (x::l) (t @ [ni;nd])
    in aux [] [tree];;
    (*val anchura : 'a arbol_binario -> 'a list = <fun>*)

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
  in Conjunto (aux c1 l2);;  
  (*val union : 'a conjunto -> 'a conjunto -> 'a list = <fun>*)

let interseccion c1 (Conjunto l2) =
  let rec aux (Conjunto l1) = function 
      [] -> []
    | h::t -> if pertenece h c1 then h::(aux c1 t) else aux (Conjunto l1) t
  in Conjunto (aux c1 l2);;
  (*val interseccion : 'a conjunto -> 'a conjunto -> 'a conjunto = <fun>*)

let diferencia (Conjunto l1) c2 = 
  let rec aux (Conjunto l2) = function 
      [] -> []
    | h::t -> if pertenece h c2 then (aux c2 t) else h::(aux (Conjunto l2) t) 
  in Conjunto (aux c2 l1);; 
  (*val diferencia : 'a conjunto -> 'a conjunto -> 'a list = <fun>*)

let rec incluido (Conjunto l1) c2 = match l1 with
    [] -> true
  | h::t -> if pertenece h c2 then incluido (Conjunto t) c2 else false;;
  (*val incluido : 'a conjunto -> 'a conjunto -> bool = <fun>*)

let igual2 c1 c2 = 
  (diferencia c1 c2) = (diferencia c2 c1);;
  (*val igual : 'a conjunto -> 'a conjunto -> bool = <fun>*)

let producto_cartesiano (Conjunto l1) (Conjunto l2) = 
  let rec aux l1 l2 l2og = match l1,l2 with
      [],_ -> []
    | (h::t),([]) -> aux t l2og l2og
    | (h1::t1), (h2::t2) -> (h1,h2)::(aux l1 t2 l2og) 
  in Conjunto (aux l1 l2 l2);;
  (*val producto_cartesiano : 'a conjunto -> 'b conjunto -> ('a * 'b) conjunto = <fun>*)


let list_of_conjunto (Conjunto l) = l;;
  (*val list_of_conjunto : 'a conjunto -> 'a list = <fun>*)
