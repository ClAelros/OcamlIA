(*Bon il va falloir quoi -> une grille de jeu de taille gérable des 0 pour les positions prenable et 2 "joueurs" le chat qu'on representera surement par c et la souris 
represente par s 
Donc je pense qu'il va falloir faire des type (comme struct certainement en C) pour au moins la grille et pour les joueurs 
Faut gerer comment les joeurs peuvent se deplacer aussi, faire une fonction pour ça 
Je pense qu'il faut commencer le jeu comme si on voulait faire un jeu pour 2 joeurs pour l'architecture puis y introduire l'IA qui utilisera les fonctions deja creee
Par contre la vraie question est comment on code un projet en OCaml -> comment on fait un main ? (peut etre avec le let () =), comment faire après peut etre avec des .mli 
Je pense fait un code bourrin avec toutes les fonctions et apres met au propre *)

(*Petit commentaire sur les tableaux :
   Pour faire un tableau 1 d Array.init taille (fun i -> i+1) fonction de construction et ça doit marcher avec une constante
   Pour faire un tableau 2d Array.make_matrix dimx dimy (nb_ligne nb_colonne) constante a quoi c'est egal, j'ai essaye avec une fonction mais ca marche pas 
   Pour modifier la valeur d'un tableau t : t.(x).(y) <- val *)

(*Cours Ocaml -> https://www.lri.fr/~filliatr/ens/compil/repro/ocaml-2x1.pdf
              -> https://ocaml.gelez.xyz/VIII/1-le-type-list/
              -> http://sdz.tdct.org/sdz/ocaml-pour-les-zeros.html
              -> https://v2.ocaml.org/api/Array.html
              -> http://www.lsv.fr/~fthire/teaching/2016-2017/programmation-1/1/corrections/libraries/libraries.pdf*)

(*
type player = {
  nb_move : int; 
  moves : int list; 
}

type player_v2 = {
  diago : int; 
  horizontal : int;
  vertical : int; 
}
*)

(*Petite idee mettre les endroits ou on peut jouer en rouge pour chaque tours -> a voir l'affichage plus tard*)
(*En Ocaml ou plus generalement en prog fonctionnelle il n'y a que des fonctions mathematique lorsqu'on code on doit toujours avoir les schemas suivant en tete :
   A->func->B
   x->f->f(x)
la prog fonc. renvoie toujours quelque chose de base il n'y a pas d'affectation ou de modification de valeur en prog fonc.*)

(* type mouvement = {  (*Peut etre refaire le type mouvement car mouvement enfaite c'est soit vertical soit horizontal soit *)
  vertical : int array; 
  horizontal : int array;
  diago_left : int array; 
  diago_right : int array 
} *)

(* type mouvement = {  (* pas possible de mettre les diagos sinon des mouvement style cavalier ne seront pas possibles, donc daans la logique ici l(horizontale t le vertical sont toujours liees les mouvement sont donc des vecteurs )*)
  horizontal : int array;
  vertical : int array;
  taille : int array (*ce sera un tableau de [|taille_min, taille_max|] si max = min alors qu'une taille*)
} *)

type vect = 
  Horizontal of int
  | Vertical of int 
  | Sum of vect * vect 
  | Opp of vect 

type mouvement = {
  move : vect;
  taille_min : int;
  taille_max : int (*On mettra peut etre un truc pour faire directement le symetrique de ce move*)
}
(*en realite plus que de faire un type mouvement ce serait plus interessent de faire un type vecteur car comme ca on a qu'a definir les vecteurs de base et le reste c'est des additions de ces vecteurs*)

type player = {
  nb_move : int;
  genre : int; (*chat ou souris*)
  moves : mouvement list;
  mutable pos_x : int;
  mutable pos_y : int 
}

let no_move = Array.make 3 0

(* let v = Array.make 3 2 in 
let h = Array.make 3 5 in 
let dl = Array.make 3 1 in 
let dr = Array.make 3 6 in 
let m = {vertical = v; horizontal = h; diago_left = dl; diago_right = dr} *)

let board_game = fun nb_lignes nb_colonnes -> 
  Array.make_matrix nb_lignes nb_colonnes 0

let start_pos = fun pos_mouse_x pos_mouse_y pos_cat_x pos_cat_y bd ->
  bd.(pos_mouse_x).(pos_mouse_y) <- 1 ; (*ici il faut le point virgule sinon ca marche pas*)
  bd.(pos_cat_x).(pos_cat_y) <- 2 

let display_board = fun bd ->
  for i=0 to Array.length bd - 1 do 
    Printf.printf "|";
    for j=0 to Array.length bd.(0) -1 do
      if bd.(i).(j) = 1 then Printf.printf "s|" else 
        (if bd.(i).(j) = 2 then Printf.printf "c|" else Printf.printf " |")
    done; (*Pas oublier le ; apres chaque putain de "intruction"*)
    Printf.printf "\n"
  done

let display_board2 = fun bd ->
  for i=0 to Array.length bd - 1 do 
    for j=0 to Array.length bd.(0) -1 do
      if bd.(i).(j) = 1 then Printf.printf "s " else 
        (if bd.(i).(j) = 2 then Printf.printf "c " else Printf.printf "¤ ")
    done; (*Pas oublier le ; apres chaque putain de "intruction"*)
    Printf.printf "\n"
  done

let rec init_player_move = fun nb_move l1 -> (*faire une fonction recursive dans la fonction pour pouvoir initialiser la liste ect*)
  if nb_move = 0 then l1 
  else 
    let i = 2 in
      init_player_move (nb_move-1) (i::l1) 

let rec init_player_move_v2 = fun nb_move l1 ->  (* faire un printf en diasnt que ne pas vouloir une direction = mettre que des 0 dans cette direction *)
  if nb_move = 0 then l1
  else 
    let v = Scanf.scanf "%d %d %d\n" (fun x y z -> [|x; y; z|]) in
    let h = Scanf.scanf "%d %d %d\n" (fun x y z -> [|x; y; z|]) in 
    let d_l = Scanf.scanf "%d %d %d\n" (fun x y z -> [|x; y; z|]) in 
    let d_r = Scanf.scanf "%d %d %d\n" (fun x y z -> [|x; y; z|]) in   (*Si on veut pouvoir faire plusieurs scnaf d'affiler il faut mettre les \n derriere pour le recup après chaque entree *)
    let new_move = {vertical = v; horizontal = h; diago_left = d_l; diago_right = d_r} in  (*Comment mettre les printf dans la fonction mais plus important comment afficher au bon moment ?*)
    init_player_move_v2 (nb_move-1) (new_move::l1)

(* let rec init_player_move_v3 = fun nb_move l1 ->  (* faire un printf en diasnt que ne pas vouloir une direction = mettre que des 0 dans cette direction *)
  if nb_move = 0 then l1
  else 
    Printf.printf "Veuillez entrer les 3 valeurs vertical\n";
    let v = Scanf.scanf "%d %d %d" (fun x y z -> [|x; y; z|]) in
    Printf.printf "Veuillez entrer les 3 valeurs horizontal\n";
    let h = Scanf.scanf "%d %d %d" (fun x y z -> [|x; y; z|]) in 
    Printf.printf "Veuillez entrer les 3 valeurs diago_gauche\n";
    let d_l = Scanf.scanf "%d %d %d" (fun x y z -> [|x; y; z|]) in 
    Printf.printf "Veuillez entrer les 3 valeurs diago_droite\n"
    let d_r = Scanf.scanf "%d %d %d" (fun x y z -> [|x; y; z|]) in
    let new_move = {vertical = v; horizontal = h; diago_left = d_l; diago_right = d_r} in 
    init_player_move_v2 (nb_move-1) (new_move::l1) *)

(*Ce que je propose pour les mouvement pour chaque player faire un fonction qui a chaque tour determine les coup jouable dans un tableau ou une liste a voir puis les
pour l'ia pas besoin de faire un affichage mais quand on veut jouer nous affiche les possibilites en x et y qu'on doit rentrer ou juste on rentre un mouvement avec une direction un sens et des chiffres pour jouer
une autre fonctions verifiera s'il est valide on peut aussi proposer les mouvement au joueurs *)

(*Une question se pose donc -> Pour chaque player suivant les mouvement donnees il peut y avoir plein de possiblites la question est donc 
   -> Comment stocker ses possibilités sachant que les tableau ont leur limite car on ne peut pas les agrandir 
    -> si je fais une liste de tableaux de 2 avec x et y c'est peut etre pas trop con
    -> je pense que le mieux pour chaque est plus de renvoyer les mouvement possible et faire une autre fonctions qui a chaque mouvement associe la position -> certainement une fonction cle*)

let make_vect = fun x y ->
  Sum(Horizontal x, Vertical y)

let rec init_player_move_v4 = fun nb_move l1 ->
  if nb_move = 0 then l1 
  else 
    let h = Scanf.scanf "%d\n" (fun x -> x) in 
    let v = Scanf.scanf "%d\n" (fun x -> x) in 
    let t_max = Scanf.scanf "%d\n" (fun x -> x) in 
    let t_min = Scanf.scanf "%d\n" (fun x -> x) in 
    let new_move = {move = make_vect h v; taille_min = t_min; taille_max = t_max} in 
    init_player_move_v4 (nb_move-1) (new_move::l1)

(* let move_to_position = fun m -> 


let possible_move = fun p ->   (*Faire 2 fonctions -> une qui trie une liste selon une condition un autre qui fait la condition*)
  let rec p_m = fun l1 l2 p1 -> 
    math *)





(*main*)
let () =
  let bd = board_game 5 5 in 
    start_pos 0 0 4 4 bd; 
    display_board2 bd;

(*test*)









