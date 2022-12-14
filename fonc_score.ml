type position = {
  i : int; (*position sur les lignes*)
  j : int (*position sur les colonnes*)
}

let abs_val value = 
  if value < 0 then
    -value
  else
    value 

let calcul_score = fun pos_cat pos_mouse ->
  abs_val (pos_cat.i - pos_mouse.i) + abs_val (pos_cat.j - pos_mouse.j)
  (* je propose (pos_cat.i - pos_mouse.i)**2+(pos_cat.j - pos_mouse.j)**2 (car c'est proportionnel à la distance)*)


let () =
  let start_pos_mouse = {i = 0; j = 0} in 
  let start_pos_cat = {i = 5; j = 5} in 
  let result = calcul_score start_pos_cat start_pos_mouse in
  Printf.printf "%d \n" result