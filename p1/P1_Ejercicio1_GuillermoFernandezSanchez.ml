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
Conjunto [Estado "1"; Estado "3"]);;


*****************************************************************************
 *
 * Un automata finito viene definido por la 5-tupla AF = (Q,E,q0,d,F) donde: 
 *    Q  = conjunto de estados      -> estado conjunto                       
 *    E  = alfabeto de entrada      -> simbolo conjunto                      
 *    q0 = estado inicial           -> estado                                
 *    d  = funcion  de transicion   -> arco_af conjunto                      
 *    F  = conjunto estados finales -> estado conjunto                       
 *
 *****************************************************************************)

let es_afne (Af (estados, simbolos, e_ini, arcos, e_fin)) = 
  let rec aux = function 
      Conjunto [] -> false
    | _ -> true
  in aux (avanza (Terminal "") estados (Af (estados, simbolos, e_ini, arcos, e_fin)));;
(*si presenta una epsilon transición, podemos afirmar que se trata de un afne*)


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
*)

(*devuelve una lista de simbolos de las transiciones salientes del estado, 
  un simbolo puede estar repetidossi hay mas de una transicion con dicho simbolo*)
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
Af (Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
Conjunto [Terminal "a"; Terminal "b"],
Estado "0",
Conjunto [Arco_af (Estado "0", Estado "1", Terminal "a");
Arco_af (Estado "0", Estado "0", Terminal "b");
Arco_af (Estado "1", Estado "1", Terminal "b");
Arco_af (Estado "1", Estado "2", Terminal "a");
Arco_af (Estado "2", Estado "0", Terminal "a");
Arco_af (Estado "3", Estado "3", Terminal "a");
Arco_af (Estado "3", Estado "3", Terminal "b");
Arco_af (Estado "2", Estado "3", Terminal "b")],
Conjunto [Estado "1"; Estado "3"])
*)

let es_afd (Af (Conjunto estados, simbolos, e_ini, arcos, e_fin)) =
  let rec aux estados_por_visitar estados_visitados = match estados_por_visitar with
      [] -> true 
    | h::t-> let simbols = (get_simbols h arcos) in 
             if not ((List.length simbols) = (cardinal (conjunto_of_list(simbols)))) then false (*comprobación de si hay simbolos repetidos*)
             else if not (igual (conjunto_of_list(simbols)) simbolos) then false
             else let rec aux2 simbols2 estados_por_visitar_temp estados_visitados_temp = match simbols2 with
                      [] -> aux estados_por_visitar_temp estados_visitados_temp
                    | h1::t1 -> let destino = avanza h1 (Conjunto [h]) (Af (Conjunto estados, simbolos, e_ini, arcos, e_fin)) in
                              if (cardinal destino) != 1 then false else let estadoDestino = (List.hd (list_of_conjunto(destino))) in
                                 if not (pertenece estadoDestino (Conjunto estados)) 
                                    then false 
                                    else if (List.mem estadoDestino estados_visitados_temp) then aux2 t1 estados_por_visitar_temp estados_visitados_temp
                                                                                       else aux2 t1 (estadoDestino::estados_por_visitar_temp) (estadoDestino::estados_visitados_temp) 
                  in aux2 simbols t estados_visitados
  in aux [e_ini] [e_ini];;
  
