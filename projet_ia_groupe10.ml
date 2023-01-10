#use "topfind"
#require "graphics"
#require "unix"

(* Définition des constantes et des types du projet *)
let nb_l = ref 0
let nb_c = ref 0
let case_size = 50

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
  role : int; (* chat ou souris *) (* 1 pour souris 2 pour chat *)
  genre : piece; 
  mutable pos_i : int;
  mutable pos_j : int 
}

type position = {
  i : int; (* position sur les lignes *)
  j : int (* position sur les colonnes *)
}

type score = {
  pos : position ; 
  point : int 
}  

type 'a tree = 
  Leaves of 'a 
| Nodmin of 'a tree list * position * position (* pos_max pos_min *)
| Nodmax of 'a tree list * position * position (* pos_max pos_min *) 

let v1 = Vertical 1 
let v2 = Horizontal 1 
let v3 = Sum(v1, v2)
let v4 = Sum(v1,(Opp v2))
let v5 = Opp v1 
let v6 = Opp v2
let v7 = Opp v3 
let v8 = Opp v4 
let v9 = Sum(v3,v1)
let v10 = Sum(v3,v2)
let v11 = Sum(v4,v1)
let v12 = Sum(v4,(Opp v2))
let v13 = Opp v9
let v14 = Opp v10
let v15 = Opp v11
let v16 = Opp v12

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
let m17 = {vecteur = v9; taille_min = 1; taille_max =1}
let m18 = {vecteur = v10; taille_min = 1; taille_max =1}
let m19 = {vecteur = v11; taille_min = 1; taille_max =1}
let m20 = {vecteur = v12; taille_min = 1; taille_max =1}
let m21 = {vecteur = v13; taille_min = 1; taille_max =1}
let m22 = {vecteur = v14; taille_min = 1; taille_max =1}
let m23 = {vecteur = v15; taille_min = 1; taille_max =1}
let m24 = {vecteur = v16; taille_min = 1; taille_max =1}

let reine = {name = "Reine"; move = [m1;m2;m3;m4;m5;m6;m7;m8]}
let roi = {name = "Roi"; move = [m9;m10;m11;m12;m13;m14;m15;m16]}
let fou = {name = "Fou"; move = [m3;m4;m7;m8]}
let tour = {name = "Tour"; move = [m1;m2;m5;m6]}
let cavalier = {name = "Cavalier"; move = [m17;m18;m19;m20;m21;m22;m23;m24]}

let p1 = {role = 1; genre = reine; pos_i = 0; pos_j = 0}

let impossible_pos = {i = -1; j = -1}
let cat_position = ref impossible_pos


(* Cette fonction prend en entrée deux nombres x et y et retourne un vecteur *)
let make_vect = fun x y ->
  if x > 0 && y >0 then Sum(Horizontal x, Vertical y)
  else if x < 0 && y >0 then Sum(Opp (Horizontal (-x)), Vertical y)
  else if x > 0 && y <0 then Sum(Horizontal x, Opp (Vertical (-y)))
  else Sum(Opp (Horizontal (-x)), Opp (Vertical (-y))) 

(* Cette fonction prend en entrée un vecteur et renvoie sa valeur horizontale *)
let rec read_vect_h = fun v -> 
  match v with
  Horizontal x -> x
  | Vertical y -> 0
  | Sum (x, y) -> (read_vect_h x)+(read_vect_h y)
  | Opp v -> (-1)*(read_vect_h v)

(* Cette fonction prend en entrée un vecteur et renvoie sa valeur verticale *)
let rec read_vect_v = fun v -> 
  match v with
  Horizontal x -> 0
  | Vertical y -> y
  | Sum (x, y) -> (read_vect_v x)+(read_vect_v y)
  | Opp v -> (-1)*(read_vect_v v)


(* Définition des fonctions pour traiter les mouvements possibles d'un personnage *)
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

(* Cette fonction renvoie la liste des mouvements possibles d'un joueur en fonction de sa position actuelle *)
let possible_move = fun joueur -> 
  let rec view_list = fun lst return -> 
    match lst with 
    [] -> return 
    | h::t -> let new_taille = new_taille_move h (joueur.pos_i) (joueur.pos_j) in 
            if new_taille != 0 then let new_move = {vecteur = h.vecteur; taille_min = h.taille_min; taille_max = new_taille} in view_list t (new_move::return)
            else view_list t return 
  in view_list (joueur.genre.move) [] 

(* Définition des fonctions pour passer des mouvements aux positions *)
let rec concatenate = fun lst1 lst2 -> 
  match lst1 with 
  [] -> lst2 
  | h::t -> h :: concatenate t lst2 

(* Cette fonction prend en paramètres un mouvement m, une position initiale en abscisse et une position initiale en ordonnée.
   Elle renvoie la liste des positions successives que prendra un objet en suivant le mouvement m *)  
let move_to_pos = fun m pos_i pos_j ->   
  let rec constr = fun k -> 
    if k < m.taille_min then []
    else 
      let h = read_vect_h m.vecteur in 
      let v = read_vect_v m.vecteur in
      let new_pos = {i = pos_i + k*v; j = pos_j + k*h} in  
      new_pos :: constr (k-1)
  in constr m.taille_max  

(* Cette fonction renvoie la liste des positions possibles pour un joueur donné *)
let cat_on_the_road = fun lst_pos -> 
  if List.mem (!cat_position) lst_pos then 
    let rec build_new_lst = fun lst return -> 
      match lst with 
      [] -> return  
      | h::t -> if h = (!cat_position) then build_new_lst t return else build_new_lst t (h::return)
    in build_new_lst lst_pos [] 
  else lst_pos

let possible_pos = fun joueur -> 
  let lst_move = possible_move joueur in 
  let rec build_list = fun lst_m return -> 
    match lst_m with 
    [] -> return 
    | h::t -> let lst_pos = move_to_pos h joueur.pos_i joueur.pos_j in build_list t (concatenate lst_pos return)
  in let lst_pos = build_list lst_move [] in 
  cat_on_the_road lst_pos 


(* Définition des fonctions de vérification des bonnes conditions du jeu *)
let move_player = fun joueur pos -> 
  joueur.pos_i <- pos.i ;
  joueur.pos_j <- pos.j 

(* Cette fonction vérifie si une position donnée est présente dans une liste de positions *)
let verif_pos = fun lst_pos pos -> 
  List.mem pos lst_pos

(* Cette fonction renvoie la position de l'élément pos dans la liste lst_pos, ou -1 s'il n'y est pas *)
let number_pos = fun lst_pos pos -> 
  let rec match_lst = fun lst k -> 
    match lst with
    [] -> -1 
    | h::t -> if h = pos then k else match_lst t (k+1)
  in match_lst lst_pos 1 

(* Cette fonction vérifie si le chat a gagné en comparant la liste des positions possibles du chat et la position actuelle de la souris *)
let is_win = fun cat mouse -> 
  let lst_pos_cat = possible_pos cat in 
  let pos_mouse = {i = mouse.pos_i; j = mouse.pos_j} in 
  verif_pos lst_pos_cat pos_mouse 

(* Cette fonction retourne l'élément à la i-ème position de la liste lst *)
let elt_of_lst = fun i lst -> 
  let rec match_lst = fun lst x ->
    match lst with 
    [] -> impossible_pos
    | h::t -> if x = i then h else match_lst t (x+1)
  in match_lst lst 1


(* Définition des fonctions d'affichage *)

(* Fonction pour dessiner une case du damier *)
let draw_case x y color =
  Graphics.set_color color;
  Graphics.fill_rect (x * case_size) (y * case_size) case_size case_size

(* Fonction pour dessiner le damier *)
let draw_board nb_lignes nb_colonnes =
  for i = 0 to nb_colonnes do
    for j = 0 to nb_lignes do
      if (i + j) mod 2 = 0 then
        draw_case i j Graphics.black
      else
        draw_case i j Graphics.white
    done
  done

(* Fonction pour dessiner un point sur le damier *)
let draw_point x y color =
  Graphics.set_color color;
  Graphics.fill_circle ((x * case_size) + (case_size / 2)) ((y * case_size) + (case_size / 2)) (case_size / 2)

let char_to_num key max_pos =
  match key with
  | '0' -> 0
  | 'a' -> 1
  | 'b' -> 2
  | 'c' -> 3
  | 'd' -> 4
  | 'e' -> 5
  | 'f' -> 6
  | 'g' -> 7
  | 'h' -> 8
  | 'i' -> 9
  | 'j' -> 10
  | 'k' -> 11
  | 'l' -> 12
  | 'm' -> 13
  | 'n' -> 14
  | 'o' -> 15
  | 'p' -> 16
  | 'q' -> 17
  | 'r' -> 18
  | 's' -> 19
  | 't' -> 20
  | 'u' -> 21
  | 'v' -> 22
  | 'w' -> 23
  | 'x' -> 24
  | 'y' -> 25
  | 'z' -> 26
  | 'A' -> 27
  | 'B' -> 28
  | 'C' -> 29
  | 'D' -> 30
  | 'E' -> 31
  | 'F' -> 32
  | 'G' -> 33
  | 'H' -> 34
  | 'I' -> 35
  | 'J' -> 36
  | 'K' -> 37
  | 'L' -> 38
  | 'M' -> 39
  | 'N' -> 40
  | 'O' -> 41
  | 'P' -> 42
  | 'Q' -> 43
  | 'R' -> 44
  | 'S' -> 45
  | 'T' -> 46
  | 'U' -> 47
  | 'V' -> 48
  | 'W' -> 49
  | 'X' -> 50
  | 'Y' -> 51
  | 'Z' -> 52
  | '1' -> 53
  | '2' -> 54
  | '3' -> 55
  | '4' -> 56
  | '5' -> 57
  | '6' -> 58
  | '7' -> 59
  | '8' -> 60
  | '9' -> 61
  | _ -> max_pos + 1

let num_to_char i =
  match i with
  | 0 -> '0'
  | 1 -> 'a'
  | 2 -> 'b'
  | 3 -> 'c'
  | 4 -> 'd'
  | 5 -> 'e'
  | 6 -> 'f'
  | 7 -> 'g'
  | 8 -> 'h'
  | 9 -> 'i'
  | 10 -> 'j'
  | 11 -> 'k'
  | 12 -> 'l'
  | 13 -> 'm'
  | 14 -> 'n'
  | 15 -> 'o'
  | 16 -> 'p'
  | 17 -> 'q'
  | 18 -> 'r'
  | 19 -> 's'
  | 20 -> 't'
  | 21 -> 'u'
  | 22 -> 'v'
  | 23 -> 'w'
  | 24 -> 'x'
  | 25 -> 'y'
  | 26 -> 'z'
  | 27 -> 'A'
  | 28 -> 'B'
  | 29 -> 'C'
  | 30 -> 'D'
  | 31 -> 'E'
  | 32 -> 'F'
  | 33 -> 'G'
  | 34 -> 'H'
  | 35 -> 'I'
  | 36 -> 'J'
  | 37 -> 'K'
  | 38 -> 'L'
  | 39 -> 'M'
  | 40 -> 'N'
  | 41 -> 'O'
  | 42 -> 'P'
  | 43 -> 'Q'
  | 44 -> 'R'
  | 45 -> 'S'
  | 46 -> 'T'
  | 47 -> 'U'
  | 48 -> 'V'
  | 49 -> 'W'
  | 50 -> 'X'
  | 51 -> 'Y'
  | 52 -> 'Z'
  | 53 -> '1'
  | 54 -> '2'
  | 55 -> '3'
  | 56 -> '4'
  | 57 -> '5'
  | 58 -> '6'
  | 59 -> '7'
  | 60 -> '8'
  | 61 -> '9'
  | _ -> ' '

let draw_pos = fun lst_pos ->
  let rec match_pos = fun lst i ->
     match lst with
     | [] -> ()
     | h :: t -> 
         begin
           Graphics.set_color Graphics.blue;
           Graphics.moveto (h.j*case_size + case_size/3 + 2) (((!nb_l-1)-h.i)*case_size + case_size/4);
           Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
           Graphics.draw_char (num_to_char i);
           match_pos t (i+1)
         end in
   match_pos lst_pos 1

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

let display_nodmin = fun pos_max pos_min -> 
  Printf.printf "pos_max = (%d, %d) " (pos_max.i) (pos_max.j);
  Printf.printf "pos_min = (%d, %d) " (pos_min.i) (pos_min.j);
  print_endline "Nodemin"

let display_nodmax = fun pos_max pos_min -> 
  let () = Printf.printf "pos_max = (%d, %d) " (pos_max.i) (pos_max.j) in
  let () = Printf.printf "pos_min = (%d, %d) " (pos_min.i) (pos_min.j) in
  print_endline "Nodemax"

let rec print_tree_v5 = fun t ->
  match t with 
  Leaves x -> Printf.printf "%d " x 
  | Nodmax (lst, pos_max, pos_min) -> let _ = List.map (fun t -> print_tree_v5 t) lst in let () = display_nodmax pos_max pos_min in print_endline " "
  | Nodmin (lst, pos_max, pos_min) -> let _ = List.map (fun t -> print_tree_v5 t) lst in let () = display_nodmin pos_max pos_min in print_endline " "
  

(* Cette fonction initialise une liste de mouvements en demandant à l'utilisateur de saisir les informations de chaque mouvement.
   Si l'utilisateur décide d'arrêter, la fonction renvoie la liste de mouvements créée *)
let init_lst_move = fun () -> 
  let rec init_lst_move_rec = fun x lst -> 
    if x = 0 then lst 
    else 
      let () = print_endline "Veuillez entrer la composante horizontale du vecteur du mouvement" in 
      let h = Scanf.scanf "%d\n" (fun x -> x) in
      let () = print_endline "Veuillez entrer la composante verticale du vecteur du mouvement " in
      let v = Scanf.scanf "%d\n" (fun x -> x) in
      let () = print_endline "Veuillez entrer la taille maximale du mouvement (>= 1)" in 
      let t_max = Scanf.scanf "%d\n" (fun x -> x) in
      let () = print_endline "Veuillez entrer la taille minimale du mouvement (>= 1)" in
      let t_min = Scanf.scanf "%d\n" (fun x -> x) in
      if (t_max < 1 || t_min < 1) then 
        let () = print_endline "Mouvement non valide, veuillez recommencer" in 
        init_lst_move_rec 1 lst 
      else
        let new_move = {vecteur = make_vect h v; taille_min = t_min; taille_max = t_max} in
        let () = print_endline "Voulez-vous entrer un autre mouvement ? Tapez 1 pour continuer, ou 0 pour arrêter" in 
        let y = Scanf.scanf "%d\n" (fun x -> x) in 
        init_lst_move_rec y (new_move::lst)
  in init_lst_move_rec 1 []  

(* Cette fonction initialise un joueur en demandant à l'utilisateur de rentrer une liste de mouvements et un nom de pièce.
   Elle crée ensuite un objet "joueur" avec ces informations, ainsi que des coordonnées initiales de position (0,0) et un rôle prédéfini (en paramètre de la fonction) *)
let init_player = fun r -> 
  let () = print_endline "Veuillez entrer la liste des mouvements de votre joueur :" in 
  let lst_move = init_lst_move () in 
  let () = print_endline "Veuillez entrer le nom de la pièce qui représente votre joueur :" in 
  let nom = Scanf.scanf "%s\n" (fun x->x) in 
  let i = 0 in 
  let j = 0 in 
  let init_piece = {name = nom; move = lst_move} in 
  {role = r; genre = init_piece; pos_i = i; pos_j = j} 

(* Cette fonction permet de choisir le joueur qui représentera soit la souris, soit le chat dans le jeu, et de personnaliser ou choisir un joueur déjà existant.
   Si le choix est mauvais, la fonction se relance *)
   let rec choose_player = fun r -> 
    let () = if r =1 then print_endline "Veuillez choisir le joueur qui représentera la souris : \nPour personnaliser un nouveau joueur : tapez 1, pour choisir un joueur déjà existant : tapez 2"  else print_endline "Veuillez choisir le joueur qui représentera le chat : \nPour personnaliser un nouveau joueur : tapez 1, pour choisir un joueur déjà existant : tapez 2" in 
    let c = Scanf.scanf "%d\n" (fun x->x) in 
    if (c < 1 || c > 2) then let () = print_endline "Mauvais choix, recommencez" in choose_player r 
    else 
      if c = 1 then init_player r 
      else 
        let () = print_endline "Vous avez le choix : \nRoi -> 1 \nReine -> 2 \nFou -> 3 \nTour -> 4 \nCavalier -> 5" in 
        let temp = Scanf.scanf "%d\n" (fun x->x) in 
        match temp with
        1 -> {role = r; genre = roi; pos_i = 0; pos_j = 0}
        | 2 -> {role = r; genre = reine; pos_i = 0; pos_j = 0}
        | 3 -> {role = r; genre = fou; pos_i = 0; pos_j = 0}
        | 4 -> {role = r; genre = tour; pos_i = 0; pos_j = 0}
        | 5 -> {role = r; genre = cavalier; pos_i = 0; pos_j = 0}
        | _ -> {role = r; genre = roi; pos_i = 0; pos_j = 0}


(* Définition des fonctions pour l'IA *)

(* Définition de la fonction pour construire l'arbre *)
let abs_val value = 
  if value < 0 then
    -value
  else
    value 

let calcul_score1 = fun pos_cat pos_mouse ->
  (pos_cat.i - pos_mouse.i)*(pos_cat.i - pos_mouse.i) + (pos_cat.j - pos_mouse.j)*(pos_cat.j - pos_mouse.j)

let calcul_score2 = fun pos_cat pos_mouse ->
  2*((pos_cat.i - pos_mouse.i)*(pos_cat.i - pos_mouse.i) + (pos_cat.j - pos_mouse.j)*(pos_cat.j - pos_mouse.j)) - abs_val ((!nb_c/2) - pos_mouse.i) - abs_val( (!nb_l/2) - pos_mouse.j) 

let grille_score = fun pos_j2 pos_j1 p_turn prof_node -> 
  if !p_turn = 0 then
    if !prof_node = 0 then
      500 + calcul_score2 pos_j2 pos_j1
    else
      500 + calcul_score2 pos_j1 pos_j2
  else
    if !prof_node = 0 then
      500 - calcul_score2 pos_j1 pos_j2
    else
      500 - calcul_score2 pos_j2 pos_j1

let build_tree = fun player_max player_min profondeur player_turn -> 
  let pos_max = {i = player_max.pos_i; j = player_max.pos_j} in
  let pos_min = {i = player_min.pos_i; j = player_min.pos_j} in
  let prof_nodmax = profondeur mod 2 in 
  let prof_node = ref 0 in 
  let rec build_tree_rec = fun pos_maxi pos_mini profondeur -> 
    let () = prof_node := if (profondeur mod 2) = prof_nodmax then 0 else 1 in   
    let p_max = {role = player_max.role; genre = player_max.genre; pos_i = pos_maxi.i; pos_j = pos_maxi.j} in
    let p_min = {role = player_min.role; genre = player_min.genre; pos_i = pos_mini.i; pos_j = pos_mini.j} in
    if profondeur = 0 then
      let lst_pos = if !prof_node = 0 then possible_pos p_max else possible_pos p_min in
      let pos_autre = if !prof_node = 0 then pos_mini else pos_maxi in 
      let grille_score_tree = fun pos -> 
        grille_score pos_autre pos player_turn prof_node
      in let lst_score = List.map (grille_score_tree) lst_pos in 
      List.map (fun x -> Leaves(x)) lst_score 
    else 
      let lst_pos = if !prof_node = 0 then possible_pos p_max else possible_pos p_min in
      let pos_autre = if !prof_node = 0 then pos_mini else pos_maxi in
      if !prof_node = 0 then 
        List.map(fun x -> Nodmin((build_tree_rec x pos_autre (profondeur-1)), x, pos_autre)) lst_pos
      else
        List.map(fun x -> Nodmax((build_tree_rec pos_autre x (profondeur-1)), pos_autre, x)) lst_pos 
  in  Nodmax((build_tree_rec pos_max pos_min profondeur), pos_max, pos_min)


(* Définition des fonctions pour analyser l'arbre *)
let match_tree_nodmax = fun t -> 
  match t with 
  Nodmax (lst,_,_) -> lst 
  | _ -> []

let match_tree_pos_max = fun t ->
  match t with
  Nodmin (_, pos_max, _) -> pos_max 
  | _ -> impossible_pos 

let elt_of_lst_tree = fun i lst -> 
  let rec match_lst = fun lst x ->
    match lst with 
    [] -> Nodmax([], impossible_pos, impossible_pos)
    | h::t -> if x = i then h else match_lst t (x+1)
  in match_lst lst 1

let max_lst = fun lst_max -> 
  let rec max = fun lst x -> 
    match lst with 
    [] -> x 
    | h::t -> if h>x then max t h else max t x 
  in max lst_max 0 


(* Définition des fonctions de traitement de l'arbre *)
let count1=ref 0
let rec minimax t =
  match t with 
  Leaves x -> let ()  = count1 := !count1 +1 in x
  | Nodmin (lst, _, _) -> List.fold_left (fun x y -> let z = minimax y in if x<z then x else z) (max_int) (lst) 
  | Nodmax (lst, _, _) -> List.fold_left (fun x y -> max x (minimax y)) (-max_int) (lst)  

let count2=ref 0
let rec alphabeta t alpha beta =
  match t with 
  Leaves s -> let () = count2 := !count2 +1 in s 
  | Nodmin (lst, _, _) -> 
    let rec loop = fun a s lst_n -> 
      match lst_n with 
      [] -> s 
      | h::q -> 
        let temp = min s (alphabeta h a s) in 
        if temp <= a then temp 
        else loop a temp q 
    in loop alpha beta lst 
  | Nodmax (lst, _, _) -> 
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
  Leaves s -> let () = count3 := !count3 +1 in s 
  | Nodmin (lst, _, _) | Nodmax (lst, _, _) ->
    let rec loop = fun lst_n s -> 
      match lst_n with 
      [] -> s 
      | h::q ->
        let temp = max s (-(negamax h (-beta) (-s))) in 
        if temp >= beta then temp
        else loop q temp 
    in loop lst alpha 

let application_of_alpha_beta = fun t -> 
  alphabeta t (-max_int) max_int

let application_of_negamax = fun t -> 
  negamax t (-max_int) max_int

let play_ia = fun player_max player_min prof player_turn -> 
  let tree = build_tree player_max player_min prof player_turn in 
  let lst_tree = match_tree_nodmax tree in
  let score_tree = List.map (fun t -> minimax t) lst_tree in
  let m = max_lst score_tree in 
  let k = number_pos score_tree m in
  let node = elt_of_lst_tree k lst_tree in 
  let new_pos = match_tree_pos_max node in 
  {pos = new_pos; point = m}

(* Cette fonction permet de jouer au jeu en demandant à l'utilisateur de choisir une position sur la grille où se déplacer.
   Si la position choisie n'est pas valide, le joueur doit rejouer.
   Si l'utilisateur entre 0, la partie s'arrête.
   Sinon, le joueur qui a joué (soit le chat, soit la souris) se déplace et si l'un d'eux gagne, la boucle de jeu se termine *)
let play = fun mouse cat ia prof view_ia -> 
  let () = print_endline "Veuillez entrer le numéro de la position pour jouer ou 0 pour quitter" in 
  let gg = ref true in 
  let quit = ref true in 
  let turn = ref 1 in
  let round = ref 0 in 
  while (!gg && !quit) do 
    if !round = 30 then 
      begin
        Graphics.clear_graph ();
        Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
        Graphics.set_color Graphics.black;
        Graphics.moveto (210) (600);
        Graphics.draw_string "LIMITE DE ROUNDS ATTEINTE";
        Graphics.moveto (315) (400);
        Graphics.draw_string "SOURIS GAGNE";
        Graphics.moveto (100) (200);
        Graphics.draw_string "PRESSEZ N'IMPORTE QUELLE TOUCHE POUR QUITTER";
        Graphics.synchronize ();
        let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
        Graphics.close_graph ();
        gg := false;
      end
    else 
      begin          
        cat_position := {i = cat.pos_i; j = cat.pos_j};
        turn := (!turn + 1) mod 2;
        let actuel_player = if !turn=0 then mouse else cat in 
        let pos_lst = possible_pos actuel_player in
        let max_pos = List.length pos_lst in 
        Printf.printf "Round n°%d\n" (!round); 
        display_v4 mouse cat pos_lst; 
        display_lst_pos_finale pos_lst;
        if !turn =0 then print_endline "Souris, à toi de jouer" else print_endline "Chat, à toi de jouer";
        (* On efface l'écran avant de dessiner le damier et le point *)
        Graphics.clear_graph ();
        draw_board (!nb_l-1) (!nb_c-1);
        draw_point (cat.pos_j) ((!nb_l-1) - cat.pos_i) Graphics.red;
        draw_point (mouse.pos_j) ((!nb_l-1) - mouse.pos_i) Graphics.green;
        Graphics.synchronize ();
        if (!turn) = ia || ia = 2 then
          let player_max = actuel_player in 
          let player_min = if !turn = 0 then cat else mouse in 
          let pos_max_tree = play_ia player_max player_min prof turn in 
          let pos_first_tree = play_ia player_max player_min 1 turn in 
          let new_pos = if (pos_max_tree.point) >= (pos_first_tree.point) then (pos_max_tree.pos) else (pos_first_tree.pos) in 
          if new_pos = impossible_pos then 
            let () = print_endline "Erreur de l'IA" in 
            quit := false 
          else
            let _ = if view_ia = 1 then Graphics.wait_next_event [Graphics.Key_pressed] else Graphics.wait_next_event [Graphics.Poll] in
            let () = move_player actuel_player new_pos in 
            let () = round := (!round+1) in 
            if actuel_player.role = 1 then 
              if is_win cat mouse then
                begin              
                  Graphics.clear_graph ();
                  draw_board (!nb_l-1) (!nb_c-1);
                  draw_point (cat.pos_j) ((!nb_l-1) - cat.pos_i) Graphics.red;
                  draw_point (mouse.pos_j) ((!nb_l-1) - mouse.pos_i) Graphics.green;
                  Graphics.set_font "-*-fixed-medium-r-semicondensed--80-*-*-*-*-*-iso8859-1";
                  Graphics.set_color Graphics.blue;
                  Graphics.moveto (35) (160);
                  Graphics.draw_string "GAME OVER";
                  Graphics.synchronize ();
                  Unix.sleep 3;
                  Graphics.clear_graph ();
                  Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
                  Graphics.set_color Graphics.black;
                  Graphics.moveto (325) (500);
                  Graphics.draw_string "CHAT GAGNE";
                  Graphics.moveto (100) (300);
                  Graphics.draw_string "PRESSEZ N'IMPORTE QUELLE TOUCHE POUR QUITTER";
                  Graphics.synchronize ();
                  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
                  Graphics.close_graph (); 
                  gg := false;
                end
              else ()
            else()  
        else  
          begin
            draw_pos pos_lst;
            Graphics.synchronize ();
            let key = Graphics.read_key () in
            let new_pos = char_to_num key max_pos in
            if (new_pos < 1 || new_pos > max_pos) then 
              if new_pos = 0 then quit:=false 
              else let () = print_endline "Position impossible veuillez rejouer" in turn := (!turn+1) mod 2 
            else 
              let play_pos = elt_of_lst new_pos pos_lst in 
              if play_pos = impossible_pos then print_endline "Mauvaise touche, veuillez rejouer" 
              else 
                let () = move_player actuel_player play_pos in 
                let () = round := (!round+1) in 
                if actuel_player.role = 1 then 
                  if is_win cat mouse then
                    begin          
                      Graphics.clear_graph ();
                      draw_board (!nb_l-1) (!nb_c-1);
                      draw_point (cat.pos_j) ((!nb_l-1) - cat.pos_i) Graphics.red;
                      draw_point (mouse.pos_j) ((!nb_l-1) - mouse.pos_i) Graphics.green;
                      Graphics.set_font "-*-fixed-medium-r-semicondensed--80-*-*-*-*-*-iso8859-1";
                      Graphics.set_color Graphics.blue;
                      Graphics.moveto (35) (160);
                      Graphics.draw_string "GAME OVER";
                      Graphics.synchronize ();
                      Unix.sleep 3;
                      Graphics.clear_graph ();
                      Graphics.set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
                      Graphics.set_color Graphics.black;
                      Graphics.moveto (325) (500);
                      Graphics.draw_string "CHAT GAGNE";
                      Graphics.moveto (100) (300);
                      Graphics.draw_string "PRESSEZ N'IMPORTE QUELLE TOUCHE POUR QUITTER";
                      Graphics.synchronize ();
                      let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
                      Graphics.close_graph (); 
                      gg := false;
                    end
                  else () 
                else () 
          end
      end
  done
  


(*Cette fonction demande à l'utilisateur de rentrer la taille de la grille et les positions de départ des joueurs (souris et chat).
   Elle déplace les joueurs vers ces positions et lance la partie.*)
let () =
  let () = print_endline "Veuillez rentrer la taille de la grille :\nRentrez le nombre de lignes (longueur verticale) :" in 
  let l = Scanf.scanf "%d\n" (fun x->x) in 
  let () = print_endline "Rentrez le nombre de colonnes (longueur horizontale) :" in 
  let c = Scanf.scanf "%d\n" (fun x->x) in 
  let () = nb_c := c in 
  let () = nb_l := l in 
  let mouse = choose_player 1 in 
  let cat = choose_player 2 in 
  let () = print_endline "Veuillez maintenant choisir les positions de départ : \nRentrez la position de la souris :\ni :" in 
  let i_mouse = Scanf.scanf "%d\n" (fun x->x) in
  let () = print_endline "j : " in 
  let j_mouse = Scanf.scanf "%d\n" (fun x->x) in
  let () = print_endline "Rentrez la position du chat : \ni :" in 
  let i_cat = Scanf.scanf "%d\n" (fun x->x) in 
  let () = print_endline "j : " in 
  let j_cat = Scanf.scanf "%d\n" (fun x->x) in
  let start_pos_mouse = {i = i_mouse; j = j_mouse} in 
  let start_pos_cat = {i = i_cat; j = j_cat} in 
  let () = move_player mouse start_pos_mouse in
  let () = move_player cat start_pos_cat in 
  let () = print_endline "Quelle configuration souhaitez-vous utiliser ? \nMouse IA vs Cat player -> 0 \nMouse player vs Cat IA -> 1 \nMouse IA vs Cat IA -> 2 \nMouse player vs Cat player -> 3" in 
  let ia = Scanf.scanf "%d\n" (fun x->x) in 
  if ia = 3 then 
    begin
      Graphics.open_graph " 800x800";
      play mouse cat ia 0 0;
    end
  else 
    begin
      let () = print_endline "Veuillez choisir la profondeur de l'arbre créé par l'IA" in 
      let prof = Scanf.scanf "%d\n" (fun x->x) in
      let () = print_endline "Voulez-vous appuyer sur une touche pour faire jouer l'IA ? \nOui -> 1 \nNon -> 0" in 
      let view_ia = Scanf.scanf "%d\n" (fun x->x) in
      Graphics.open_graph " 800x800";
      play mouse cat ia prof view_ia 
    end