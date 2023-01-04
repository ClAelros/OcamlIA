(*Definition des constantes et des types du projet*)
let nb_l = ref 0
let nb_c = ref 0

type vect = 
  Horizontal of int
  | Vertical of int 
  | Sum of vect * vect 
  | Opp of vect 

type mouvement = {
  vecteur : vect;
  taille_min : int;
  taille_max : int 
}

type piece = {
  name : string;
  move : mouvement list
}

type player = {
  role : int; (*chat ou souris*) (*1 pour souris 2 pour chat*)
  genre : piece; 
  mutable pos_i : int;
  mutable pos_j : int 
}

type position = {
  i : int; (*position sur les lignes*)
  j : int (*position sur les colonnes*)
}

type 'a tree = 
  Leaves of 'a 
| Nodmin of 'a tree list * position * position (*pos_max pos_min*)
| Nodmax of 'a tree list * position * position (*pos_max pos_min*) 


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

let p1 = {role = 1; genre = reine; pos_i = 0; pos_j = 0}
let p2 = {role = 2; genre = roi; pos_i = 4; pos_j = 4} 

let impossible_pos = {i = -1; j = -1}


(*Definition des fonctions pour creer et lire des vecteurs*)
let make_vect = fun x y ->
  if x > 0 && y >0 then Sum(Horizontal x, Vertical y)
  else if x < 0 && y >0 then Sum(Opp (Horizontal (-x)), Vertical y)
  else if x > 0 && y <0 then Sum(Horizontal x, Opp (Vertical (-y)))
  else Sum(Opp (Horizontal (-x)), Opp (Vertical (-y))) 

let rec read_vect_h = fun v -> 
  match v with
  Horizontal x -> x
  | Vertical y -> 0
  | Sum (x, y) -> (read_vect_h x)+(read_vect_h y)
  | Opp v -> (-1)*(read_vect_h v)

let rec read_vect_v = fun v -> 
  match v with
  Horizontal x -> 0
  | Vertical y -> y
  | Sum (x, y) -> (read_vect_v x)+(read_vect_v y)
  | Opp v -> (-1)*(read_vect_v v)


(*Definition des fonctions pour traiter les mouvement possibles d'un personnage*)
let new_taille_move = fun m i j -> 
  let p_j = read_vect_h m.vecteur in 
  let p_i = read_vect_v m.vecteur in 
  let taille = m.taille_max in 
  let rec test_taille = fun t -> 
    if t<m.taille_min then 0
    else 
      if ((j + t*p_j >= 0) && (j + t*p_j <= (!nb_c-1))) && ((i + t*p_i >= 0) && (i + t*p_i <= (!nb_l-1))) then t 
      else test_taille (t-1) 
  in test_taille taille 

let possible_move = fun joueur -> 
  let rec view_list = fun lst return -> 
    match lst with 
    [] -> return 
    | h::t -> let new_taille = new_taille_move h (joueur.pos_i) (joueur.pos_j) in 
            if new_taille != 0 then let new_move = {vecteur = h.vecteur; taille_min = h.taille_min; taille_max = new_taille} in view_list t (new_move::return)
            else view_list t return 
  in view_list (joueur.genre.move) [] 


(*Definition des fonctions pour passer des mouvements au positions*)
let rec concatenate = fun lst1 lst2 -> 
  match lst1 with 
  [] -> lst2 
  | h::t -> h :: concatenate t lst2 

let move_to_pos = fun m pos_i pos_j ->   
  let rec constr = fun k -> 
    if k < m.taille_min then []
    else 
      let h = read_vect_h m.vecteur in 
      let v = read_vect_v m.vecteur in
      let new_pos = {i = pos_i + k*v; j = pos_j + k*h} in  
      new_pos :: constr (k-1)
  in constr m.taille_max  

let possible_pos = fun joueur -> 
  let lst_move = possible_move joueur in 
  let rec build_list = fun lst_m return -> 
    match lst_m with 
    [] -> return 
    | h::t -> let lst_pos = move_to_pos h joueur.pos_i joueur.pos_j in build_list t (concatenate lst_pos return)
  in build_list lst_move []


(*Definition des fonctions de verification des bonnes conditions du jeu*)
let move_player = fun joueur pos -> 
  joueur.pos_i <- pos.i ;
  joueur.pos_j <- pos.j 

let verif_pos = fun lst_pos pos -> 
  List.mem pos lst_pos

let number_pos = fun lst_pos pos -> 
  let rec match_lst = fun lst k -> 
    match lst with
    [] -> -1 
    | h::t -> if h = pos then k else match_lst t (k+1)
  in match_lst lst_pos 1 

let is_win = fun cat mouse -> 
  let lst_pos_cat = possible_pos cat in 
  let pos_mouse = {i = mouse.pos_i; j = mouse.pos_j} in 
  verif_pos lst_pos_cat pos_mouse 

let elt_of_lst = fun i lst -> 
  let rec match_lst = fun lst x ->
    match lst with 
    [] -> impossible_pos
    | h::t -> if x = i then h else match_lst t (x+1)
  in match_lst lst 1


(*Definition des fonctions d'affichage (temporaire avant d'avoir le graphics)*)
let display_pos = fun pos -> 
  Printf.printf "(%d, %d)\n" pos.i pos.j

let display_lst_pos_finale = fun lst_pos ->
  let rec display_list = fun lst k -> 
    match lst with 
    [] -> Printf.printf "." 
    | h::t -> let () = Printf.printf "%d ->" k in 
      let () = display_pos h in 
      display_list t (k+1)
  in let () = display_list lst_pos 1 in 
  print_newline ()

let display_v4 = fun j1 j2 lst_pos -> 
  for i=0 to !nb_l-1 do 
    for j=0 to !nb_c-1 do 
      let pos = {i = i; j = j} in 
      if verif_pos lst_pos pos then 
        let k = number_pos lst_pos pos in 
        if k>9 then Printf.printf "%d " k
        else Printf.printf "%d  " k 
      else if i = j1.pos_i && j = j1.pos_j then 
        if j1.role = 1 then Printf.printf "s  "
        else Printf.printf "c  "
      else if i = j2.pos_i && j = j2.pos_j then
        if j2.role = 1 then Printf.printf "s  "
        else Printf.printf "c  "
      else Printf.printf "¤  "
    done;
    Printf.printf "   %d\n" i;
  done;
  Printf.printf "\n";
  for j=0 to !nb_c-1 do 
    if j>9 then Printf.printf "%d " j 
    else Printf.printf "%d  " j;
  done;
  print_newline ()  
  

(*Definition des fonctions qui permettent de creer son personnage*)
let init_lst_move = fun () -> 
  let rec init_lst_move_rec = fun x lst -> 
    if x = 0 then lst 
    else 
      let () = print_endline "Veuillez entrer la composante horizontale du vecteur du mouvement" in 
      let h = Scanf.scanf "%d\n" (fun x -> x) in
      let () = print_endline "Veuillez entrer la composante verticale du vecteur du mouvement " in
      let v = Scanf.scanf "%d\n" (fun x -> x) in
      let () = print_endline "Veuillez entrer la taille maximum du mouvement (>= 1)" in 
      let t_max = Scanf.scanf "%d\n" (fun x -> x) in
      let () = print_endline "Veuillez entrer la taille minimale du mouvement (>=1)" in
      let t_min = Scanf.scanf "%d\n" (fun x -> x) in
      if (t_max < 1 || t_min < 1) then 
        let () = print_endline "Mouvement non valide, veuillez recommencer" in 
        init_lst_move_rec 1 lst 
      else
        let new_move = {vecteur = make_vect h v; taille_min = t_min; taille_max = t_max} in
        let () = print_endline "Voulez vous entrer un autre mouvement ? Tapez 1 pour continuer 0 pour arreter" in 
        let y = Scanf.scanf "%d\n" (fun x -> x) in 
        init_lst_move_rec y (new_move::lst)
  in init_lst_move_rec 1 []  

let init_player = fun r -> 
  let () = print_endline "Veuillez entrer la liste de mouvement pour votre joueur :" in 
  let lst_move = init_lst_move () in 
  let () = print_endline "Veuillez entree le nom de la piece qui represente votre joueur" in 
  let n = Scanf.scanf "%s\n" (fun x->x) in 
  let i = 0 in 
  let j = 0 in 
  let p = {name = n; move = lst_move} in 
  {role = r; genre = p; pos_i = i; pos_j = j} 

let rec choose_player = fun r -> 
  let () = if r =1 then print_endline "Veuillez choisir le joueur qui représentera la souris : \nPour personalisez un joueur taper 1 pour choisir un joueur deja existant taper 2"  else print_endline "Veuillez choisir le joueur qui représentera le chat : \nPour personalisez un joueur taper 1 pour choisir un joueur deja existant taper 2" in 
  let c = Scanf.scanf "%d\n" (fun x->x) in 
  if (c < 1 || c > 2) then let () = print_endline "Mauvais choix, recommencer" in choose_player r 
  else 
    if c = 1 then init_player r 
    else 
      let () = print_endline "Vous avez le choix : \nRoi -> 1 \nReine -> 2" in 
      let p = Scanf.scanf "%d\n" (fun x->x) in 
      if p = 1 then {role = r; genre = roi; pos_i = 0; pos_j = 0} else {role = r; genre = reine; pos_i = 0; pos_j = 0}


(*Definition des fonctions pour l'IA*)


(*Definition de la fonction pour construire l'arbre*)
let grille_score = fun pos_j1 pos_j2 -> 
  pos_j1.i + pos_j2.i

let build_tree_v2 = fun player_max player_min profondeur -> 
  let pos_max = {i = player_max.pos_i; j = player_max.pos_j} in
  let pos_min = {i = player_min.pos_i; j = player_min.pos_j} in
  let prof_nodmax = profondeur mod 2 in 
  let p = ref 0 in 
  let rec build_tree_rec_v2 = fun pos_maxi pos_mini profondeur -> 
    let () = p := if (profondeur mod 2) = prof_nodmax then 0 else 1 in   
    let p_max = {role = player_max.role; genre = player_max.genre; pos_i = pos_maxi.i; pos_j = pos_maxi.j} in
    let p_min = {role = player_min.role; genre = player_min.genre; pos_i = pos_mini.i; pos_j = pos_mini.j} in
    if profondeur = 0 then
      let lst_pos = if !p = 0 then possible_pos p_max else possible_pos p_min in
      let pos_autre = if !p = 0 then pos_mini else pos_maxi in 
      let grille_score_tree = fun pos -> 
        grille_score pos_autre pos 
      in let lst_score = List.map (grille_score_tree) lst_pos in 
      List.map (fun x -> Leaves(x)) lst_score 
    else 
      let lst_pos = if !p = 0 then possible_pos p_max else possible_pos p_min in
      let pos_autre = if !p = 0 then pos_mini else pos_maxi in
      if !p = 0 then 
        List.map(fun x -> Nodmin((build_tree_rec_v2 x pos_autre (profondeur-1)), x, pos_autre)) lst_pos
      else
        List.map(fun x -> Nodmax((build_tree_rec_v2 pos_autre x (profondeur-1)), pos_autre, x)) lst_pos 
  in  Nodmax((build_tree_rec_v2 pos_max pos_min profondeur), pos_max, pos_min)

(*Definition des fonctions d'heuristique (*a completer*)*)

(*Definition des focntions de traitement de l'arbre (Pour l'instant en commentaire je vais les modifs après)*)
(* let count1=ref 0
let rec minimax t =
  match t with 
  Leaf x -> let ()  = count1 := !count1 +1 in x
  | Nodmin lst -> List.fold_left (fun x y -> let z = minimax y in if x<z then x else z) (max_int) (lst) 
  | Nodmax lst -> List.fold_left (fun x y -> max x (minimax y)) (-max_int) (lst)  

let count2=ref 0
let rec alphabeta t alpha beta =
  match t with 
  Leaf s -> let () = count2 := !count2 +1 in s 
  | Nodmin lst -> 
    let rec loop = fun a s lst_n -> 
      match lst_n with 
      [] -> s 
      | h::q -> 
        let temp = min s (alphabeta h a s) in 
        if temp <= a then temp 
        else loop a temp q 
    in loop alpha beta lst 
  | Nodmax lst -> 
    let rec loop = fun s b lst_n -> 
      match lst_n with 
      [] -> s 
      | h::q -> 
        let temp = max s (alphabeta h s b) in 
        if temp >= b then temp 
        else loop temp b q 
    in loop alpha beta lst   

let count3=ref 0
let rec negamax t alpha beta =
  match t with 
  Leaf s -> let () = count3 := !count3 +1 in s 
  | Nodmin lst | Nodmax lst ->
    let rec loop = fun lst_n s -> 
      match lst_n with 
      [] -> s 
      | h::q ->
        let temp = max s (-(negamax h (-beta) (-s))) in 
        if temp >= beta then temp
        else loop q temp 
    in loop lst alpha  *)


(*Fonction de jeu*)
let play = fun mouse cat -> 
  let () = print_endline "Veuillez entrer le numéro de la position pour jouer ou 0 pour quitter" in 
  let gg = ref true in 
  let quit = ref true in 
  let p = ref 1 in
  while (!gg && !quit) do 
    let () = p := (!p+1) mod 2 in
    let actuel_player = if !p=0 then mouse else cat in 
    let pos_lst = possible_pos actuel_player in
    let max_pos = List.length pos_lst in  
    let () = display_v4 mouse cat pos_lst in  
    let () = display_lst_pos_finale pos_lst in 
    let () = if !p =0 then print_endline "Souris a toi de jouer" else print_endline "Chat a toi de jouer" in 
    let new_pos = Scanf.scanf "%d\n" (fun x->x) in (*C'est ici qu'il faudra l'appel a une fonction pour l'ia*)
    if (new_pos < 1 || new_pos > max_pos) then 
      if new_pos = 0 then quit:=false 
      else let () = print_endline "Position impossible veuillez rejouer" in p := (!p+1) mod 2 
    else 
      let play_pos = elt_of_lst new_pos pos_lst in 
      if play_pos = impossible_pos then print_endline "Mauvaise touche veuillez rejouer" 
      else 
        let () = move_player actuel_player play_pos in 
        if actuel_player.role = 1 then 
          if is_win cat mouse then gg:=false 
          else () 
        else () 
  done


(*main*)
let () =
  let () = print_endline "Veuillez rentrer la taille de la grille :\nRentrer le nombre de ligne (longueur verticale) :" in 
  let l = Scanf.scanf "%d\n" (fun x->x) in 
  let () = print_endline "Rentrer le nombre de colonne (longueur horizontale) :" in 
  let c = Scanf.scanf "%d\n" (fun x->x) in 
  let () = nb_c := c in 
  let () = nb_l := l in 
  let mouse = choose_player 1 in 
  let cat = choose_player 2 in 
  let () = print_endline "Veuillez maintenant choisir les positions de departs: \nRentrer la position de la souris :\ni :" in 
  let i_mouse = Scanf.scanf "%d\n" (fun x->x) in (*regarder plus tard ici aussi si on fait une verif de pos*) 
  let () = print_endline "j : " in 
  let j_mouse = Scanf.scanf "%d\n" (fun x->x) in
  let () = print_endline "Rentrer la position du chat : \ni :" in 
  let i_cat = Scanf.scanf "%d\n" (fun x->x) in (*regarder plus tard ici aussi si on fait une verif de pos*) 
  let () = print_endline "j : " in 
  let j_cat = Scanf.scanf "%d\n" (fun x->x) in
  let start_pos_mouse = {i = i_mouse; j = j_mouse} in 
  let start_pos_cat = {i = i_cat; j = j_cat} in 
  let () = move_player mouse start_pos_mouse in
  let () = move_player cat start_pos_cat in 
  play mouse cat 

  
  
