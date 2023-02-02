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



(*Ejercicio 2: primero_que_cumple*)

let rec primero_que_cumple f = function 
    []-> raise Not_found
  | h::t -> if f(h) then h else primero_que_cumple f t;;

  (*tipo: val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun>*)

let existe f l = 
  try (let _ = primero_que_cumple f l in true) with Not_found -> false;;

  (*tipo: val existe : ('a -> bool) -> 'a list -> bool = <fun>*)

