type vect = 
  Horizontal of int
  | Vertical of int 
  | Sum of vect * vect 
  | Opp of vect 

type mouvement = {
  move : vect;
  taille_min : int;
  taille_max : int 
}

type piece = {
  name : string;
  move : mouvement list
}

type player = {
  (*nb_move : int;*)
  role : int; (*chat ou souris*)  (*faire des flushs -> a voir comment en ocaml -> flush stdout *)
  genre : piece; 
  mutable pos_x : int;
  mutable pos_y : int 
}

type position = {
  x : int;
  y : int 
}

let board_game = fun nb_lignes nb_colonnes -> 
  Array.make_matrix nb_lignes nb_colonnes 0

let start_pos = fun pos_mouse_x pos_mouse_y pos_cat_x pos_cat_y bd -> (*a revoir peut etre une liste de joueur suivant si on veut jouer avec plusieur chat ou souriss*)
  bd.(pos_mouse_x).(pos_mouse_y) <- 1 ; 
  bd.(pos_cat_x).(pos_cat_y) <- 2 

let display_board2 = fun bd -> (*voir si on a une grille*)
  for i=0 to Array.length bd - 1 do 
    for j=0 to Array.length bd.(0) -1 do
      if bd.(i).(j) = 1 then Printf.printf "s " else 
        (if bd.(i).(j) = 2 then Printf.printf "c " else Printf.printf "¤ ")
    done; 
    Printf.printf "\n"
  done
  

let make_vect = fun x y ->
  Sum(Horizontal x, Vertical y)

let rec read_vect_x = fun v -> 
  math v with
  Horizontal x -> x
  | Vertical y -> 0
  | Sum (x, y) -> (read_vect_x x)+(read_vect_x y)
  | Opp v -> (-1)*(read_vect_x v)

let rec read_vect_y = fun v -> 
  math v with
  Horizontal x -> 0
  | Vertical y -> y
  | Sum (x, y) -> (read_vect_x x)+(read_vect_x y)
  | Opp v -> (-1)*(read_vect_x v)

let rec init_player_move_v4 = fun nb_move l1 -> (*init piece*)
  if nb_move = 0 then l1 
  else 
    let h = Scanf.scanf "%d\n" (fun x -> x) in 
    let v = Scanf.scanf "%d\n" (fun x -> x) in 
    let t_max = Scanf.scanf "%d\n" (fun x -> x) in 
    let t_min = Scanf.scanf "%d\n" (fun x -> x) in 
    let new_move = {move = make_vect h v; taille_min = t_min; taille_max = t_max} in 
    init_player_move_v4 (nb_move-1) (new_move::l1)

let rec possible_move = fun lst x y nb_x nb_y ->
  match lst with 
  [] -> []
  | h::t -> let p_x = read_vect_x h in let p_y = read_vect_y h in 
                      if (x+p_x >= 0 &&)


(*main*)
let () =
  let bd = board_game 5 5 in 
    start_pos 0 0 4 4 bd; 
    display_board2 bd;

(*sys.argv ou un modele sys qui fait ca ou on peut avoir des arguments optionnels
   plus que de rentrer les infos à la suite
   Module Arg arg.parse -> prend en ligne de commande -> 3arguments liste de triplets d'option fonction string doc et ..., dexuieme argument gere les arguments anonyme, 3eme petit texte qui permet de .. 
   fun x-> traitement de la string utiliser le module Arg pour les traiter les strings 
   *)

  