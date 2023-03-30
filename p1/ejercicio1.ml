#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(*
Af (Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
Estado "0",
Conjunto [Arco_af (Estado "0", Estado "1", Terminal "a");
Arco_af (Estado "1", Estado "1", Terminal "b");
Arco_af (Estado "1", Estado "2", Terminal "a");
Arco_af (Estado "2", Estado "0", Terminal "a");
Arco_af (Estado "2", Estado "3", Terminal "c")],
Conjunto [Estado "1"; Estado "3"])


*****************************************************************************
 *
 * Un aut�mata finito viene definido por la 5-tupla AF = (Q,E,q0,d,F) donde: 
 *    Q  = conjunto de estados      -> estado conjunto                       
 *    E  = alfabeto de entrada      -> simbolo conjunto                      
 *    q0 = estado inicial           -> estado                                
 *    d  = funci�n  de transici�n   -> arco_af conjunto                      
 *    F  = conjunto estados finales -> estado conjunto                       
 *
 *****************************************************************************)


(*
función es_afne(automata):
  para cada estado en el automata:
    para cada transición en las transiciones del estado:
      si el símbolo de entrada de la transición es epsilon:
        devolver true
  para cada estado en el automata:
    para cada transición en las transiciones del estado:
      si el símbolo de entrada de la transición no es epsilon:
        si el estado de destino tiene épsilon-transiciones:
          devolver true
  devolver false
*)
let es_afne (Af (estados, simbolos, e_ini, arcos, e_fin)) = 
  let rec aux = function 
      Conjunto [] -> false
    | _ -> true
  in aux (avanza (Terminal "") estados (Af (estados, simbolos, e_ini, arcos, e_fin)));;


(*
función es_afn(automata):
  para cada estado en el automata:
    para cada transición en las transiciones del estado:
      si hay algún símbolo de entrada duplicado en las transiciones del estado:
        devolver true
  devolver false
  *)

  (*
Af (Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
Estado "0",
Conjunto [Arco_af (Estado "0", Estado "1", Terminal "a");
Arco_af (Estado "1", Estado "1", Terminal "b");
Arco_af (Estado "1", Estado "2", Terminal "a");
Arco_af (Estado "2", Estado "0", Terminal "a");
Arco_af (Estado "2", Estado "3", Terminal "a");
Arco_af (Estado "2", Estado "3", Terminal "c")],
Conjunto [Estado "1"; Estado "3"]);;


let get_transitions estado (Conjunto arcos) =
  let rec aux arcs transiciones = match arcs with 
      [] -> transiciones
    | (Arco_af (e, d, s))::t -> if (e=estado) then aux t (Arco_af (e, d, s)::transiciones) else (aux t transiciones)
  in aux arcos [];;
*)
let get_simbols estado (Conjunto arcos) =
  let rec aux arcs sim = match arcs with 
      [] -> sim
    | (Arco_af (e, d, s))::t -> if (e=estado) then (aux t (s::sim)) else (aux t sim)
  in aux arcos [];;

let es_afn (Af (Conjunto estados, simbolos, e_ini, arcos, e_fin)) =
  let rec aux = function
      [] -> false
    | h::t -> let simbols = (get_simbols h arcos) in 
              if (List.length simbols) = (cardinal (conjunto_of_list(simbols))) (*Comprobación de si hay algun item repetido*)
                then aux t
                else true
  in aux estados;;


(*
funcion es_afd(automata):
  simbolos_entrada_inicial = lista de símbolos de entrada de las transiciones del estado inicial del automata
  estados_visitados = [estado inicial del automata]
  para cada estado en estados_visitados:
    transiciones_estado = lista de transiciones del estado en el automata
    simbolos_entrada = lista de simbolos de entrada de las transiciones_estado
    si hay algún símbolo de entrada duplicado en simbolos_entrada:
      devuelve false
    para cada símbolo de simbolos_entrada:
      destino = estado de destino de la transición correcpondiente al símbolo
      si el destino no es un estado del autómata:
        devolver false
      si el destino no está en estados_visitados:
        añadir destino a estados_visitados
  devolver true   
*)

let es_afd (Af (Conjunto estados, simbolos, e_ini, arcos, e_fin)) =
  let simbolos_entrada_inicial = get_simbols (e_ini, arcos) in
  let estados_visitados = [e_ini] in 
  