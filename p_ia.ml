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

let m9 = {vecteur = v1; taille_min = 1; taille_max =1}
let m10 = {vecteur = v2; taille_min = 1; taille_max =1}
let m11 = {vecteur = v3; taille_min = 1; taille_max =1}
let m12 = {vecteur = v4; taille_min = 1; taille_max =1}
let m13 = {vecteur = v5; taille_min = 1; taille_max =1}
let m14 = {vecteur = v6; taille_min = 1; taille_max =1}
let m15 = {vecteur = v7; taille_min = 1; taille_max =1}
let m16 = {vecteur = v8; taille_min = 1; taille_max =1}

let reine = {name = "Reine"; move = [m1;m2;m3;m4;m5;m6;m7;m8]}
let roi = {name = "Roi"; move = [m9;m10;m11;m12;m13;m14;m15;m16]}

let p1 = {role = 1; genre = reine; pos_x = 0; pos_y = 0}
let p2 = {role = 2; genre = roi; pos_x = 1; pos_y = 1} (*a modif*)

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

let rec concatenate = fun lst1 lst2 -> 
  match lst1 with 
  [] -> lst2 
  | h::t -> h :: concatenate t lst2 

let move_to_pos = fun m pos_x pos_y -> 
  let rec constr = fun i -> 
    if i < m.taille_min then []
    else 
      let x = read_vect_x m.vecteur in 
      let y = read_vect_y m.vecteur in 
      let new_pos = {x = pos_x + i*x; y = pos_y + i*y} in 
      new_pos :: constr (i-1)
  in constr m.taille_max  

let possible_pos = fun joueur -> 
  let lst_move = possible_move joueur in 
  let rec build_list = fun lst_m return -> 
    match lst_m with 
    [] -> return 
    | h::t -> let lst_pos = move_to_pos h joueur.pos_x joueur.pos_y in build_list t (concatenate lst_pos return)
  in build_list lst_move []

let move_player = fun joueur pos -> 
  joueur.pos_x <- pos.x ;
  joueur.pos_y <- pos.y 

let verif_pos = fun lst_pos pos -> 
  List.mem pos lst_pos

let is_win = fun lst_pos_cat mouse -> 
  let pos_mouse = {x = mouse.pos_x; y = mouse.pos_y} in 
  verif_pos lst_pos_cat pos_mouse 


  

let display_v2 = fun j1 j2 -> (*voir comment changer la couleur du texte en affichage et passer en parametre une liste de pos pour les mettre en rouge*)
  for i=0 to !nb_c-1 do 
    for j=0 to !nb_l-1 do 
      if i = j1.pos_x && j = j1.pos_y then 
        if j1.role = 1 then Printf.printf "s "
        else Printf.printf "c "
      else if i = j2.pos_x && j = j2.pos_y then
        if j2.role = 1 then Printf.printf "s "
        else Printf.printf "c "
      else Printf.printf "¤ "
    done;
    Printf.printf "\n";
  done

let rec display_vect = fun v ->
  match v with 
  Horizontal x -> Printf.printf "Horizontal %d" (x)
  | Vertical y -> Printf.printf "Vertical %d" (y)
  | Sum (x,y) -> let () = Printf.printf "Sum (" in 
    let () = display_vect x in 
    let () = Printf.printf ", " in 
    let () = display_vect y in 
    Printf.printf ")"
  | Opp v -> let () = Printf.printf "Opp (" in 
    let () = display_vect v in  
    Printf.printf ")"

let display_move = fun m -> 
  let () = Printf.printf "{vecteur : " in 
  let () = display_vect m.vecteur in 
  let () = Printf.printf "; taille_min : %d; taille_max : %d}" m.taille_min m.taille_max in 
  print_newline () 

let display_lst_move = fun lst_m -> 
  let () = Printf.printf "[" in 
  let rec display_list = fun lst -> 
    match lst with
    [] -> Printf.printf "]" 
    | h::t -> let() = display_move h in 
      let () = Printf.printf "; " in 
      display_list t 
  in let () = display_list lst_m in 
  print_newline () 

let display_pos = fun pos -> 
  Printf.printf "(%d, %d)\n" pos.y pos.x   (*a voir si on met pos.x pos.y ou comme ca*)

let display_lst_pos = fun lst_pos ->
  let () = Printf.printf "[" in 
  let rec display_list = fun lst -> 
    match lst with 
    [] -> Printf.printf "]" 
    | h::t -> let () = display_pos h in 
      let () = Printf.printf "; " in 
      display_list t 
  in let () = display_list lst_pos in 
  print_newline () 

let display_v3 = fun j1 j2 -> (*voir comment changer la couleur du texte en affichage et passer en parametre une liste de pos pour les mettre en rouge*)
  for i=0 to !nb_c-1 do 
    for j=0 to !nb_l-1 do 
      if i = j1.pos_x && j = j1.pos_y then 
        if j1.role = 1 then Printf.printf "s "
        else Printf.printf "c "
      else if i = j2.pos_x && j = j2.pos_y then
        if j2.role = 1 then Printf.printf "s "
        else Printf.printf "c "
      else Printf.printf "¤ "
    done;
    Printf.printf "   %d\n" i;
  done;
  Printf.printf "\n";
  for j=0 to !nb_l-1 do 
    Printf.printf "%d " j;
  done;
  print_newline ()  (*a droite c'est les y et a gauche les x juste changer le display des position sinon*)



(*main*)
let () =
  nb_c := 8;
  nb_l := 8;
  display_vect v1; 
  print_newline (); 
  display_vect v2; 
  print_newline ();
  display_vect v3; 
  print_newline ();
  display_vect v7; 
  print_newline ();
  display_move m3; 
  let lst = possible_move p1 in 
  display_lst_move lst; 
  let pos = {x = 0; y = 0} in 
  display_pos pos; 
  let lst2 = possible_pos p1 in 
  display_lst_pos lst2; 
  display_v3 p1 p2


(*sys.argv ou un modele sys qui fait ca ou on peut avoir des arguments optionnels
   plus que de rentrer les infos à la suite
   Module Arg arg.parse -> prend en ligne de commande -> 3arguments liste de triplets d'option fonction string doc et ..., dexuieme argument gere les arguments anonyme, 3eme petit texte qui permet de .. 
   fun x-> traitement de la string utiliser le module Arg pour les traiter les strings 
   *)

  