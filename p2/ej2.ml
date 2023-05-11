(*********************************************************
 *
 * Práctica realizada por:  
 *     
 * Guillermo Fernández Sánchez - guillermo.fernandezs
 *
 *********************************************************)
#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;



(*
gic: "S A B C; a b c; S; S -> A B | B C; A -> B A | a; B -> C C | b; C -> A B | a;"
cadena: "bbab"
*)


let get_nonter sim reglas =  (*devuelve los simbolos no terminales que tienen reglas que acaban en un terminal*)
  let rec aux l = function (*reglas*)
      [] -> l 
    | (Regla_gic(s,ls))::t -> if List.mem sim ls 
                              then aux (s::l) t
                              else aux l t
  in aux [] reglas;;


let llenarfila1 cadena reglas = (* rellena la fila 1 de la matriz*)
  let rec aux l = function (*lista simbolos*) (*l -> lista de listas de no terminales*)
      [] ->  List.rev l
    | h::t -> aux ((get_nonter h reglas)::l) t
  in aux [] cadena 
;;


let siguiente_fila triangulo reglas = [];;
(*Funcion sin implementar por falta de tiempo. El objetivo de esta funcion seria comprobar 
  el triangulo y observar cual es la siguiente linea a rellenar y, despues, rellenarla
  *)


 let cyk cadena (Gic(Conjunto nonter, Conjunto ter, Conjunto reglas, s_ini )) = 
  if (not (es_fnc (Gic(Conjunto nonter, Conjunto ter, Conjunto reglas, s_ini )))) || (cadena=[])
    then false 
  else 
    let fila1 = llenarfila1 cadena reglas 
    in let rec aux tri = 
      if ((List.length tri) == (List.length cadena))
      then List.mem s_ini (List.hd (List.hd tri)) 
      else let nueva_fila = siguiente_fila tri reglas 
           in aux (List.append nueva_fila tria)
    in aux [fila1]
;;