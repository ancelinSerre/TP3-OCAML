(* +------------------------------------+ *)
(* + TP3 - Programmation Fonctionnelle  + *)
(* + Baptiste BOUVIER et Ancelin SERRE  + *)
(* +------------------------------------+ *)

(* On commence par déclarer 2 listes pour nos tests *)
let liste = 2::7::1::9::20::[];;
let liste2 = 1::2::3::4::[];;

(*  Exercice 36  *)
(* +-----------+ *)    

(* 
  Fonction permettant de concaténer deux listes.

  @params 
    l1, l2 : list
  @return
    list (résultat de la concaténation de l1 et l2)
*)
let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | i::next -> i::append next l2;;

(* Vérification du bon fonctionnement de la fonction append *)
let res = 2::7::1::9::20::1::2::3::4::[];;
let _ = assert ((append liste liste2)= res);;

(* 
  Fonction permettant de renverser l'ordre des éléments d'une liste.

  @params
    l : list
  @return
    list (correspond à la liste l avec ses éléments dans l'ordre inverse)
*)
let rec reverse l =
  match l with
  | [] -> []
  | i::next -> append (reverse next) (i::[]);;

(* Vérification de la fonction reverse *)
let res = 20::9::1::7::2::[];;
let _ = assert ((reverse liste) = res);;

let _ = assert (reverse (reverse liste) = liste);;

let res = 4::3::2::1::[];;
let _ = assert (reverse liste2 = res);;

(*  Exercice 37  *)
(* +-----------+ *) 

(* 
  Fonction identique à renverser mais en version termimale récursive.
  On utilise donc ici une fonction auxiliaire (un accumulateur).
  Cela nous permet d'éviter un stack overflow.

  @params
    l, l2 : list
  @return
    list (correspond à la liste l avec ses éléments dans l'ordre inverse)
*)
let renverser_tr l l2 =
  let rec aux acc l =
    match l with
    | [] -> acc
    | x::l -> aux (x::acc) l
  in
  aux l2 l;;

(* Vérification de la fonction reverse_tr *)
let res = 20::9::1::7::2::2::7::1::9::20::[];;
let _ = assert ((renverser_tr liste liste) = res);;

let res = 20::9::1::7::2::1::2::3::4::[];;
let _ = assert ((renverser_tr liste liste2) = res);;

(* Pour renverser une seule liste avec renverser_tr, on appelle la fonction *)
(* avec une liste vide en deuxieme parametre comme dans l'appelle suivant *)
renverser_tr liste [];;

let res = reverse liste;;
let _ = assert ((renverser_tr liste []) = res);;

(*  Exercice 38  *)
(* +-----------+ *) 

(* 
  Fonction permettant de générer une liste d'entier aléatoires
  de taille n.

  @params
    n : int
  @return
    list (contient n entier éléments aléatoires)
*)
let liste_alea n =
  let rec aux acc n =
    match n with
    | 0 -> acc
    | _ -> aux (Random.int(200)::acc) (n-1)
in
Random.self_init();
aux [] n;;

(*  Exercice 39  *)
(* +-----------+ *) 

(* On a besoin de générer une liste pour la suite de l'exercice. *)
let alea = liste_alea 10000;;

(* Mesure du temps mis par la fonction reverse pour la liste. *)
let deb = Sys.time() in
    let _ = reverse (alea)
    in Sys.time() -. deb;;

(* Mesure du temps mis par la fonction reverse_tr pour la liste. *)
let deb = Sys.time() in
    let _ = renverser_tr (alea)
    in Sys.time() -. deb;;

(*  Exercice 40 *)
(* +-----------+ *) 

(* Déclaration du type abint qui correspond à un arbre binaire de recherche. *)
type abint = Vide | Noeud of abint * int * abint;;
(* Génération d'un arbre pour tester les fonctions des prochains exercices. *)
let abr = Noeud(Noeud(Noeud(Vide,7,Vide),9,Vide),10,Noeud(Vide,12,Vide));;

(*  Exercice 41  *)
(* +-----------+ *) 

(* 
  Fonction vérifiant si la valeur n est présent dans un noeud de l'arbre.

  @params
    arbre : abint
    n : int
  @return
    bool
*)
let rec mem arbre n =
  match arbre with
  | Vide -> false
  | Noeud(g,x,d) -> if x = n then true else (mem g n) || (mem d n);;

(* Vérification de la fonction mem. *)
let _ = assert ((mem abr 10) = true);;
let _ = assert ((mem abr 123) = false);;
let _ = assert ((mem abr 7) = true);;

(*  Exercice 42  *)
(* +-----------+ *)

(* 
  Fonction insérant la valeur dans l'arbre.

  @params
    arbre : abint
    n : int
*)
let rec insert arbre n =
  match arbre with
  | Vide -> Noeud(Vide,n,Vide)
  | Noeud(g,x,d) -> 
    if x > n then Noeud(insert g n,x,d) 
    else if x < n then Noeud(g,x,insert d n) 
    (* On ne peut pas ajouter la clé n si elle est déjà dans l'arbre. *)
    else failwith "Element deja present";;

(* Vérification de la fonction insert. *)
insert abr 1;;
let _ = assert ((mem (insert abr 1) 1) = true);;
insert abr 123;;
let _ = assert ((mem (insert abr 123) 123) = true);;
insert abr 10;;

(*  Exercice 43  *)
(* +-----------+ *)

(* 
  Fonction vérifiant si l'arbre est bien
  un arbre binaire de recherche.

  @params
    arbre : abint
  @return
    bool
*)
let rec verif arbre =
  match arbre with
  | Vide -> true
  | Noeud(g,x,d) -> 
    match (g,d) with
    | (Vide,Vide) -> true
    | (Noeud(gg,xg,dg),Vide) -> if xg < x then verif g else false;
    | (Vide, Noeud(gd,xd,dd)) -> if xd > x then verif d else false;
    | (Noeud(gg,xg,dg),Noeud(gd,xd,dd)) -> if xg < x && xd > x then (verif g) || (verif d) else false;;

(* Vérification de la fonction verif. *)
let abr2 = Noeud(Vide,5,Noeud(Vide,2,Vide));;
let _ = assert (verif abr = true);;
let _ = assert (verif abr2 = false);;

(*  Exercice 44  *)
(* +-----------+ *)

(* Fonction non-trouvé *)

(*  Exercice 45  *)
(* +-----------+ *)

(* 
  Fonction pour faciliter le tri de l'arbre.
  @params
    l : list
  @return
    abint 
*)
let rec listeVersAbr l =
  match l with
  | [] -> Vide
  | x::[] -> Noeud(Vide,x,Vide)
  | x::l -> insert (listeVersAbr l) x;;

let liste2 = 30::10::20::[];;

(* 
  Fonction pour trier l'arbre dans l'ordre croissant.
  @params
    arbre : abint
  @return
    list (liste des clés de l'arbre rangées dans l'ordre croissant.
*)
let rec triAbr arbre =
  match arbre with
  | Vide -> []
  | Noeud(Vide,x,Vide) -> x::[]
  | Noeud(g,x,Vide) -> append (triAbr g) (x::[])
  | Noeud(Vide,x,d) -> append (x::[]) (triAbr d)
  | Noeud(g,x,d) -> append (append (triAbr g) (x::[]) ) (triAbr d);;

(* Vérification de la fonction triAbr. *)
let res = 1::2::7::9::20::[];;
let _ = assert(triAbr (listeVersAbr liste) = res);;

let res = 10::20::30::[];;
let _ = assert(triAbr (listeVersAbr liste2) = res);;
