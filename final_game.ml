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
let p2 = {role = 2; genre = roi; pos_x = 4; pos_y = 4} (*a modif*)

let impossible_pos = {x = -1; y = -1}

let make_vect = fun x y ->
  Sum(Horizontal x, Vertical y)

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

let fct2 = fun m x y -> 
  let p_x = read_vect_h m.vecteur in 
  let p_y = read_vect_v m.vecteur in 
  let taille = m.taille_max in 
  let rec test_taille = fun t -> 
    if t<m.taille_min then 0
    else 
      if ((y + t*p_x >= 0) && (y + t*p_x <= (!nb_c-1))) && ((x + t*p_y >= 0) && (x + t*p_y <= (!nb_l-1))) then t 
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
      let h = read_vect_h m.vecteur in 
      let v = read_vect_v m.vecteur in
      let new_pos = {x = pos_x + i*v; y = pos_y + i*h} in  
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

let number_pos = fun lst_pos pos -> 
  let rec match_lst = fun lst i -> 
    match lst with
    [] -> -1 
    | h::t -> if h = pos then i else match_lst t (i+1)
  in match_lst lst_pos 1 

let is_win = fun cat mouse -> 
  let lst_pos_cat = possible_pos cat in 
  let pos_mouse = {x = mouse.pos_x; y = mouse.pos_y} in 
  verif_pos lst_pos_cat pos_mouse 

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
  Printf.printf "(%d, %d)\n" pos.x pos.y   

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
  print_newline ()  

let display_lst_pos_finale = fun lst_pos ->
  let rec display_list = fun lst i -> 
    match lst with 
    [] -> Printf.printf "." 
    | h::t -> let () = Printf.printf "%d ->" i in 
      let () = display_pos h in 
      display_list t (i+1)
  in let () = display_list lst_pos 1 in 
  print_newline ()

let elt_of_lst = fun i lst -> 
  let rec match_lst = fun lst x ->
    match lst with 
    [] -> impossible_pos
    | h::t -> if x = i then h else match_lst t (x+1)
  in match_lst lst 1

let display_v4 = fun j1 j2 lst_pos -> 
  for i=0 to !nb_c-1 do 
    for j=0 to !nb_l-1 do 
      let pos = {x = i; y = j} in 
      if verif_pos lst_pos pos then 
        let k = number_pos lst_pos pos in 
        if k>9 then Printf.printf "%d " k
        else Printf.printf "%d  " k 
      else if i = j1.pos_x && j = j1.pos_y then 
        if j1.role = 1 then Printf.printf "s  "
        else Printf.printf "c  "
      else if i = j2.pos_x && j = j2.pos_y then
        if j2.role = 1 then Printf.printf "s  "
        else Printf.printf "c  "
      else Printf.printf "¤  "
    done;
    Printf.printf "   %d\n" i;
  done;
  Printf.printf "\n";
  for j=0 to !nb_l-1 do 
    if j>9 then Printf.printf "%d " j 
    else Printf.printf "%d  " j;
  done;
  print_newline ()  

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
  
let init_player = fun () -> 
  let () = print_endline "Veuillez entrer le role de votre joueur : Souris : 1 Chat : 2" in 
  let r = Scanf.scanf "%d\n" (fun x->x) in 
  let () = print_endline "Veuillez entrer la liste de mouvement pour votre joueur :" in 
  let lst_move = init_lst_move () in 
  let () = print_endline "Veuillez entree le nom de la piece qui represente votre joueur" in 
  let n = Scanf.scanf "%s\n" (fun x->x) in 
  let i = 0 in 
  let j = 0 in 
  let p = {name = n; move = lst_move} in 
  {role = r; genre = p; pos_x = i; pos_y = j} 
  
let rec choose_player = fun ()-> 
  let () = print_endline "Veuillez choisir votre joueur : \nPour personalisez un joueur taper 1 pour choisir un joueur deja existant taper 2" in 
  let c = Scanf.scanf "%d\n" (fun x->x) in 
  if (c < 1 || c > 2) then let () = print_endline "Mauvais choix, recommencer" in choose_player () 
  else 
    if c = 1 then init_player () 
    else 
      let () = print_endline "Vous avez le choix : \nRoi -> 1 \nReine -> 2" in 
      let p = Scanf.scanf "%d\n" (fun x->x) in 
      if p = 1 then p1 else p2 
  

let play = fun mouse cat -> (*faudra voir qui joue en premier chat ou souris ?*) (*Faudra rajouter un compteur pour savoir quand la souris gagne*)
  let () = print_endline "Veuillez entrer le numéro de la position pour jouer ou 0 pour quitter" in 
  let gg = ref true in 
  let quit = ref true in 
  let p = ref 1 in
  while (!gg && !quit) do 
    let () = p := (!p+1) mod 2 in
    let actuel_player = if !p=0 then mouse else cat in 
    let pos_lst = possible_pos actuel_player in
    let max_pos = List.length pos_lst in  
    let () = display_v3 mouse cat in  (*ICI VOUS POUVEZ choisir votre display entre le v3 et le v4 pour le v4 faut aussi passer la pos_lst en argument tester les 2 et dites moi celui que vous préférer ;)*)
    let () = display_lst_pos_finale pos_lst in 
    let () = if !p =0 then print_endline "Souris a toi de jouer" else print_endline "Chat a toi de jouer" in 
    let new_pos = Scanf.scanf "%d\n" (fun x->x) in 
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
  nb_c := 8;
  nb_l := 8;
  play p1 p2 


(*Les petits ajouts a faire :
   -> pouvoir rentrer la position de départ de chaque joueur
   -> pouvoir créer des mouvement et des joueurs personalises V -> reste plus qu'a l'integrer dans play 
   -> pouvoir rentrer la taille de la grille
   -> faut voir aussi si le chat est un obstacle au deplacement -> pour l'instant on peut jouer sur lui 
   -> Faudra demander aussi si on vérifie toutes les entrées clavier -> exemple : pour entrer la taille de la grille vérifier si le numéro entrer est négatif ou si c'est une lettre ect 
   (facultatif)-> changer l'affichage pour mettre en rouge les positions jouables et avoir le numéro de la position directement sur la grille*)