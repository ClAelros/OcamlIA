let nb_l = ref 8
let nb_c = ref 8

type 'a tree = 
  Leaf of 'a 
| Node of 'a tree list
 

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
  role : int; (*chat ou souris*)  (*faire des flushs -> a voir comment en ocaml -> flush stdout *) (*1 pour souris 2 pour chat*)
  genre : piece; 
  mutable pos_i : int;
  mutable pos_j : int 
}

type position = {
  i : int; (*position sur les lignes*)
  j : int (*position sur les colonnes*)
}

type 'a tree_v2 = 
  Leaves of 'a 
| Nodmin of 'a tree_v2 list * position * position (*pos_max pos_min*)
| Nodmax of 'a tree_v2 list * position * position (*pos_max pos_min*) 

(* let t1= Node((Leaf 10)::(Leaf 1000)::[])
let t2= Node((Leaf 5)::(Leaf (-10))::[])
let t3= Node((Leaf 6)::[])
let t4= Node((Leaf 2)::(Leaf 8)::[])
let t5= Node((Leaf (-5))::[])
let t6= Node((Leaf 0)::[])
let t7= Node((Leaf (-1))::(Leaf (-1000))::(Leaf 100)::[])
let t8= Node((Leaf 3)::(Leaf 7)::[])
let t9= Node((Leaf (-5))::(Leaf (-3))::[])
  
let t10=Node(t1::t2::[])
let t11=Node(t3::[])
let t12=Node(t4::[])
let t13=Node(t5::t6::[])
let t14=Node(t7::t8::[])
let t15=Node(t9::[])

let t16=Node(t10::t11::[])
let t17=Node(t12::t13::[])
let t18=Node(t14::t15::[])

let t=Node(t16::t17::t18::[]) *)
let pos_neutre = {i = -1; j = -1}

let t1= Nodmin((Leaves 10)::(Leaves 100000)::[], pos_neutre, pos_neutre)
let t2= Nodmin((Leaves 5)::(Leaves (-10))::[], pos_neutre, pos_neutre)
let t3= Nodmin((Leaves 6)::[], pos_neutre, pos_neutre)
let t4= Nodmin((Leaves 2)::(Leaves 8)::[], pos_neutre, pos_neutre)
let t5= Nodmin((Leaves (-5))::[], pos_neutre, pos_neutre)
let t6= Nodmin((Leaves 0)::[], pos_neutre, pos_neutre)
let t7= Nodmin((Leaves (-1))::(Leaves (-100000))::(Leaves 100)::[], pos_neutre, pos_neutre)
let t8= Nodmin((Leaves 3)::(Leaves 7)::[], pos_neutre, pos_neutre)
let t9= Nodmin((Leaves (-5))::(Leaves (-3))::[], pos_neutre, pos_neutre)
  
let t10=Nodmax(t1::t2::[], pos_neutre, pos_neutre)
let t11=Nodmax(t3::[], pos_neutre, pos_neutre)
let t12=Nodmax(t4::[], pos_neutre, pos_neutre)
let t13=Nodmax(t5::t6::[], pos_neutre, pos_neutre)
let t14=Nodmax(t7::t8::[], pos_neutre, pos_neutre)
let t15=Nodmax(t9::[], pos_neutre, pos_neutre)

let t16=Nodmin(t10::t11::[], pos_neutre, pos_neutre)
let t17=Nodmin(t12::t13::[], pos_neutre, pos_neutre)
let t18=Nodmin(t14::t15::[], pos_neutre, pos_neutre)

let t=Nodmax(t16::t17::t18::[], pos_neutre, pos_neutre)

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

let m1test = {vecteur = v1; taille_min = 1; taille_max = 1}
let m2test = {vecteur = v2; taille_min = 1; taille_max = 1}
let m3test = {vecteur = v5; taille_min = 1; taille_max = 1}
let m4test = {vecteur = v6; taille_min = 1; taille_max = 1}

let reine = {name = "Reine"; move = [m1;m2;m3;m4;m5;m6;m7;m8]}
let roi = {name = "Roi"; move = [m9;m10;m11;m12;m13;m14;m15;m16]}
let test1 = {name = "test1"; move =[m1test; m2test]}
let test2 = {name = "test2"; move =[m3test; m4test]}

let p1 = {role = 1; genre = reine; pos_i = 0; pos_j = 0}
let p2 = {role = 2; genre = roi; pos_i = 4; pos_j = 4} (*a modif*)
let ptest1 = {role = 1; genre = test1; pos_i = 0; pos_j = 0}
let ptest2 = {role = 2; genre = test2; pos_i = 7; pos_j = 7}


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


(* let rec test_print_tree = fun t  -> 
  match t with 
  Leaf x -> Printf.printf "%d" x 
  | Node (h::q) -> 
    let () = test_print_tree h in  *)

(* let rec print_tree = fun t -> 
  let rec compute_tree_list = fun lst -> 
    match lst with 
    [] -> print_endline " "
    | h::q -> let () = print_tree h in compute_tree_list q 
  in match t with 
  Leaf x -> Printf.printf "%d" x
  | Node (h::q) -> let () = print_tree h in compute_tree_list q  
      *)

let grille_score = fun pos_j1 pos_j2 -> 
  pos_j1.i + pos_j2.i     

let test_display = fun x y ->
  let () = Printf.printf "%d" x in 
  let () = Printf.printf "%d" y in 
  print_endline "ntm"

let display_nodmin = fun pos_max pos_min -> 
  Printf.printf "pos_max = (%d, %d) " (pos_max.i) (pos_max.j);
  Printf.printf "pos_min = (%d, %d) " (pos_min.i) (pos_min.j);
  print_endline "Nodemin"

let display_nodmax = fun pos_max pos_min -> 
  let () = Printf.printf "pos_max = (%d, %d) " (pos_max.i) (pos_max.j) in
  let () = Printf.printf "pos_min = (%d, %d) " (pos_min.i) (pos_min.j) in
  print_endline "Nodemax"

let rec print_tree = fun t -> 
  match t with 
  Leaf x -> Printf.printf "%d " x 
  | Node lst -> let a = List.map (fun t -> print_tree t) lst in print_endline " " 

let rec print_tree_v2 = fun t ->
  match t with 
  Leaves x -> Printf.printf "%d " x 
  | Nodmax (lst, _, _) -> let a = List.map (fun t -> print_tree_v2 t) lst in print_endline "Nodemax "
  | Nodmin (lst, _, _) -> let a = List.map (fun t -> print_tree_v2 t) lst in print_endline "Nodemin "

(* let rec print_tree_v3 = fun t ->
  match t with 
  Leaves x -> Printf.printf "%d " x 
  | Nodmax (lst, pos_max, pos_min) -> 
    let a = List.map (fun t -> print_tree_v2 t) lst in 
    let () = Printf.printf "pos_max = (%d, %d) " (pos_max.i) (pos_max.j) in
    let () = Printf.printf "pos_min = (%d, %d) " (pos_min.i) (pos_min.j) in
    print_endline "Nodemax "
  | Nodmin (lst, pos_max, pos_min) -> 
    let a = List.map (fun t -> print_tree_v2 t) lst in 
    let () = Printf.printf "pos_max = (%d, %d) " (pos_max.i) (pos_max.j) in
    let () = Printf.printf "pos_min = (%d, %d) " (pos_min.i) (pos_min.j) in
    print_endline "Nodemin " *)

(* let rec print_tree_v4 = fun t ->
  match t with 
  Leaves x -> Printf.printf "%d " x 
  | Nodmax (lst, pos_max, pos_min) -> let a = List.map (fun t -> print_tree_v2 t) lst in print_endline "Nodemax, pos_max = (%d, %d), pos_min = (%d, %d) " (pos_max.i) (pos_max.j) (pos_min.i) (pos_min.j)
  | Nodmin (lst, pos_max, pos_min) -> let a = List.map (fun t -> print_tree_v2 t) lst in print_endline "Nodemin, pos_max = (%d, %d), pos_min = (%d, %d) " (pos_max.i) (pos_max.j) (pos_min.i) (pos_min.j) *)

let rec print_tree_v5 = fun t ->
  match t with 
  Leaves x -> Printf.printf "%d " x 
  | Nodmax (lst, pos_max, pos_min) -> let a = List.map (fun t -> print_tree_v5 t) lst in let () = display_nodmax pos_max pos_min in print_endline " "
  | Nodmin (lst, pos_max, pos_min) -> let a = List.map (fun t -> print_tree_v5 t) lst in let () = display_nodmin pos_max pos_min in print_endline " "


(*
let rec print_tree_v6 = fun t ->
  match t with 
  Leaves x -> Printf.printf "%d " x 
  | Nodmax (lst, pos_max, pos_min) -> 
    let a = List.map (fun t -> print_tree_v2 t) lst in 
    let () = Printf.printf "pos_max = (%d, %d) " (pos_max.i) (pos_max.j) in
    let () = print_newline () in
    let () = Printf.printf "pos_min = (%d, %d) " (pos_min.i) (pos_min.j) in
    let () = print_newline () in
    print_endline "Nodemax "
  | Nodmin (lst, pos_max, pos_min) -> 
    let a = List.map (fun t -> print_tree_v2 t) lst in 
    let () = Printf.printf "pos_max = (%d, %d) " (pos_max.i) (pos_max.j) in
    let () = print_newline () in
    let () = Printf.printf "pos_min = (%d, %d) " (pos_min.i) (pos_min.j) in
    let () = print_newline () in 
    print_endline "Nodemin "


let rec print_tree_v7 = fun t ->
  match t with 
  Leaves x -> Printf.printf "%d " x 
  | Nodmax (lst, _, _) -> let a = List.map (fun t -> print_tree_v2 t) lst in print_endline "Nodemax "
  | Nodmin (lst, _, _) -> let a = List.map (fun t -> print_tree_v2 t) lst in let() = test_display 42 43 in print_endline "Nodemin " *)


let build_tree = fun player_max player_min profondeur -> 
  let pos_max = {i = player_max.pos_i; j = player_max.pos_j} in
  let pos_min = {i = player_min.pos_i; j = player_min.pos_j} in 
  let p = ref 1 in 
  let rec build_tree_rec = fun pos_maxi pos_mini profondeur -> 
    let () = p := (!p+1) mod 2 in  
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
        List.map(fun x -> Nodmin((build_tree_rec x pos_autre (profondeur-1)), x, pos_autre)) lst_pos
      else
        List.map(fun x -> Nodmax((build_tree_rec pos_autre x (profondeur-1)), pos_autre, x)) lst_pos 
  in  Nodmax((build_tree_rec pos_max pos_min profondeur), pos_max, pos_min)

  
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
  
       
(*Faudra que tu te remette dans le code mais en gros t'esssayais de faire la fonction qui construit l'arbre pour ca tu le fais en recursif t'as 2 cas 
   -> le premier quand ta profondeur arrive a 0 tu construit une liste de feuilles de score mais pour l'instant c'est ici que t'as un bug a la compil donc faut que tu regarde ca 
   -> le deuxieme le cas general tu construit une liste de noeud -> je sais pas trop comment l'expliquer maois ça me paraissait logique au moment ou je l'ai ecrit
   -> Pour cette fonction t'as completement changé ta maniere de penserr cette fonction prend en argument que 2 position (ca suffit pour represente une grille) donc tu prend absolument pas d'arbre en paramatere
   Bonne chance pour la prochaine fois 
   -----> ca compile maintenant
   -> Prochaine fois faire le test de la fonction build tree puis la tester avec une petite grille et des mouvement simple au debut et l'afficher avec print_tree_v2
   -> dessiner l'arbre attendu par le test et verifier avec la fonction print tree si l'arbre est bon !! *)


(* let () = 
  nb_c := 8;
  nb_l := 8; 
  print_tree_v2 t; 
  print_tree_v5 t  *)

let () = 
nb_c := 8;
nb_l := 8; 
let first_tree = build_tree_v2 ptest1 ptest2 3 in 
let () = print_tree_v2 first_tree in 
let () = print_endline " " in 
print_tree_v5 first_tree 
  
  
