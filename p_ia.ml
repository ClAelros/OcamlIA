(*Pour installer Ocaml : https://www.youtube.com/watch?v=-yabnyUL4-U*)

let nb_l = ref 0 (*nombre de ligne *)
let nb_c = ref 0 (*nombre de colonne *)

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

let v1 = Vertical 1 
let v2 = Horizontal 1 
let v3 = Sum(v1, v2)
let v4 = Sum(v1,(Opp v2))
let v5 = Opp v1 
let v6 = Opp v2
let v7 = Opp v3 
let v8 = Opp v4 

let m1 = {vecteur = v1; taille_min = 1; taille_max =2}
let m2 = {vecteur = v2; taille_min = 1; taille_max =2}
let m3 = {vecteur = v3; taille_min = 1; taille_max =2}
let m4 = {vecteur = v4; taille_min = 1; taille_max =2}
let m5 = {vecteur = v5; taille_min = 1; taille_max =2}
let m6 = {vecteur = v6; taille_min = 1; taille_max =2}
let m7 = {vecteur = v7; taille_min = 1; taille_max =2}
let m8 = {vecteur = v8; taille_min = 1; taille_max =2}

let reine = {name = "Reine"; move = [m1;m2;m3;m4;m5;m6;m7;m8]}

let p1 = {role = 1; genre = reine; pos_x = 0; pos_y = 0}

let board_game = fun nb_lignes nb_colonnes -> 
  Array.make_matrix nb_lignes nb_colonnes 0

let start_pos = fun pos_mouse_x pos_mouse_y pos_cat_x pos_cat_y bd -> (*a revoir peut etre une liste de joueur suivant si on veut jouer avec plusieur chat ou souriss*)
  nb_l := nb_lignes;
  nb_c := nb_colonnes;
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
  match v with
  Horizontal x -> x
  | Vertical y -> 0
  | Sum (x, y) -> (read_vect_x x)+(read_vect_x y)
  | Opp v -> (-1)*(read_vect_x v)

let rec read_vect_y = fun v -> 
  match v with
  Horizontal x -> 0
  | Vertical y -> y
  | Sum (x, y) -> (read_vect_y x)+(read_vect_y y)
  | Opp v -> (-1)*(read_vect_y v)

let rec init_player_move_v4 = fun nb_move l1 -> (*init piece*)
  if nb_move = 0 then l1 
  else 
    let h = Scanf.scanf "%d\n" (fun x -> x) in 
    let v = Scanf.scanf "%d\n" (fun x -> x) in 
    let t_max = Scanf.scanf "%d\n" (fun x -> x) in 
    let t_min = Scanf.scanf "%d\n" (fun x -> x) in 
    let new_move = {move = make_vect h v; taille_min = t_min; taille_max = t_max} in 
    init_player_move_v4 (nb_move-1) (new_move::l1)

let fct2 = fun m x y -> 
  let p_x = read_vect_x m.vecteur in 
  let p_y = read_vect_y m.vecteur in 
  let taille = m.taille_max in 
  let rec test_taille = fun t -> 
    if t<m.taille_min then 0
    else 
      if ((x + t*p_x >= 0) && (x + t*p_x <= (!nb_c-1))) && ((y + t*p_y >= 0) && (y + t*p_y <= (!nb_l-1))) then t 
      else test_taille (t-1) 
  in test_taille taille 

let possible_move = fun joueur -> 
  let rec view_list = fun lst return -> 
    match lst with 
    [] -> return 
    | h::t -> let new_taille = fct2 h (joueur.pos_x) (joueur.pos_y) in 
            if new_taille != 0 then let new_move = {vecteur = h.vecteur; taille_min = h.taille_min; taille_max = new_taille} in view_list t (new_move::return)
            else view_list t return 
  in view_list (joueur.genre.move) []

  


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

  