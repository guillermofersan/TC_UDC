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


let equivalentes (Af (estados1, simbolos1, e_ini1, arcos1, e_fin1)) (Af (estados2, simbolos2, e_ini2, arcos2, e_fin2)) = 
  let rec aux cola estados_visitados = 
    if not (igual simbolos1 simbolos2) then false else match cola with
      [] -> true
    | (estado_actual1, estado_actual2)::t -> 
            if List.mem (estado_actual1,estado_actual2) estados_visitados
            then aux t estados_visitados
            else if (((pertenece estado_actual1 e_fin1) && (not (pertenece estado_actual2 e_fin2))) ||
                    ((pertenece estado_actual2 e_fin2) && (not (pertenece estado_actual1 e_fin1))))
                    then false 
                    else let rec aux2 simbolos cola2 = match simbolos with
                            [] -> aux cola2 ((estado_actual1, estado_actual2)::estados_visitados)
                          | h1::t1 -> aux2 t1 (((List.hd(list_of_conjunto(avanza h1 (Conjunto [estado_actual1]) (Af (estados1, simbolos1, e_ini1, arcos1, e_fin1))))), (List.hd(list_of_conjunto(avanza h1 (Conjunto [estado_actual2]) (Af (estados2, simbolos2, e_ini2, arcos2, e_fin2))))))::cola2)
                        in aux2 (list_of_conjunto(simbolos1)) t
  in aux [(e_ini1, e_ini2)] [];; 
