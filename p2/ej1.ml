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

(*****************************************************************************
 *
 * Una gramática independiente del contexto viene definida por la 4-tupla    
 * (N,T,P,S) donde:                                                          
 *    N = conjunto de símbolos no terminales -> simbolo conjunto             
 *    T = conjunto de símbolos terminales    -> simbolo conjunto             
 *    P = reglas de producción               -> regla_gic conjunto           
 *    S = símbolo inicial                    -> simbolo                      
 *
 *****************************************************************************)

(* Ejemplo de gic
Gic (Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"],
Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
Conjunto [
Regla_gic (No_terminal "S", [Terminal "a"; No_terminal "A"]);
Regla_gic (No_terminal "A",
[Terminal "a"; Terminal "b"; Terminal "c";
No_terminal "A"]);
Regla_gic (No_terminal "A", [Terminal "b"; No_terminal "B"]);
Regla_gic (No_terminal "B",
[Terminal "b"; Terminal "c"; No_terminal "B"]);
Regla_gic (No_terminal "B", [])],
No_terminal "S")   
*)

(* Ejemplo de gic en forma normal de Chomsky
S -> Ca D1 | Cb D2
D1 -> Cb A
D2 -> Ca B
A -> Cb S | b
B -> Ca S | a
Ca -> a
Cb -> b

gic_of_string "S D1 D2 A B Ca Cb; a b; S; S -> Ca D1 | Cb D2; D1 -> Cb A; D2 -> Ca B; A -> Cb S | b; B -> Ca S | a; Ca -> a; Cb -> b;"
*)

let es_fnc (Gic(Conjunto nonter, Conjunto ter, Conjunto reglas, s_ini )) =
  let rec aux = function 
      (*caso base*)
      []->true 

      (*Si solo hay un item en la lista de simbolos de la regla, el simbolo ha de ser terminal*)
    | (Regla_gic (_,h::[]))::t -> (List.mem h ter) && (aux t)

      (*Si hay dos item en la lista de simbolos de la regla, ambos han de ser no terminales*)
    | (Regla_gic (_,h1::h2::[]))::t ->  (List.mem h1 nonter) && (List.mem h2 nonter) && (aux t)

      (*En cualquier otro caso, la gic no está en la forma normal de chomsky*)
    | _ -> false
  in aux reglas;;



