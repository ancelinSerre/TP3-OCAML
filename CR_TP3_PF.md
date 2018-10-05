# TP3 - Programmation Fonctionnelle
**Auteurs** : Baptiste Bouvier et Ancelin Serre \
**Date** : 28/09/2018 \
**Polytech INFO4**

**Site du TP :** [lien vers le TP3](http://www-verimag.imag.fr/~wack/APF/Poly-TP-18-19.pdf) \
**Cours utiles pour ce TP:**
- [Cours 1](http://www-verimag.imag.fr/~wack/APF/Cours01.pdf)
- [Cours 2](http://www-verimag.imag.fr/~wack/APF/Cours02.pdf)
- [Cours 3](http://www-verimag.imag.fr/~wack/APF/Cours03.pdf)

*Note: un fichier OCaml (.ml) est présent dans le dépôt afin de jouer nos bouts de codes plus rapidement.*

-----
## Exercice 36
*( Renversement naïf )* \
Ecrire une fonction `renverser` qui prend en argument une liste, et renvoie la liste formée des mêmes éléments en ordre inverse. \
Cette première version devra opérer sur le principe suivant :
- Renverser la queue de la liste
- Puis placer la tête de la liste en dernière position de la queue renversée (à l’aide d’une fonction auxiliaire).

```ocaml
(* Exemple de liste *)
let liste = 2::5::6::3::1::9::[];;

let rec renverser liste =
    (* Ajoute une la liste l2 à la liste l1 *)
    let rec ajouter l1 l2 = 
        match l1 with
        | [] -> l2
        | elem::next -> elem::ajouter next l2 
    in
    match liste with
    | [] -> []
    | elem::next -> ajouter (renverser next) (elem::[]);;
```

## Exercice 37
*( Renversement récursif terminal )* \
Ecrire une fonction `renverser_tr` qui prend en argument deux listes l1 et l2, et renvoie une liste formée :
- des éléments de l1 en ordre inverse
- puis des éléments de l2 le tout sans utiliser la fonction de l’exercice précédent ni la concaténation.

>Comment utilisez-vous `renverser_tr` pour renverser une seule liste ? \
>On appelle la fonction `renverser_tr` avec la **liste que l'on veut inverser et []** ce qui fait qu'elle n'aura rien à ajouter à la liste donnée.

```ocaml
let rec renverser_tr l1 l2 = 
    match l1 with
    | [] -> l2
    | elem::next -> renverser_tr (elem::l1) next;;
```

## Exercice 38
*( Génération de tests )* \
Écrire une fonction `liste_alea`, qui prend en argument un entier n et génère une liste de taille n d’entiers tirés au hasard (peu importe la façon exacte de choisir ces entiers). \
*Vous  pouvez  utiliser  la  librairie [Random](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html)*.

```ocaml
let liste_alea entier =
    let rec aux acc entier = 
        match entier with
        | 0 -> acc
        | _ -> aux (Random.int(100000)::acc) (entier-1)
    in
    Random.self_init();
    aux [] entier;;
```

## Exercice 39 

*( Mesures )* \
Comparer les **temps d’exécution** de vos deux fonctions de renversement, *pour des listes d’entiers de tailles allant jusqu’à 10 000 éléments* (ou plus si nécessaire pour constater une différence, selon votre machine). \
\
Faire plusieurs mesures pour évaluer le coût d’exécution de vos fonctions en fonction de la taille de la liste, déterminer si ce coût dépend des valeurs présentes dans la liste, etc.
Il est possible d’obtenir des mesures précises à l’aide de la fonction `Sys.time()`

```ocaml
(* On génère une liste pour tester *)
let liste = liste_alea 10000

let temps_reverse = Sys.time() in
    let _ = renverser liste in
    Sys.time() -. temps_reverse;;

let temps_reverse_tr = Sys.time() in 
    let _ = renverser_tr liste in 
    Sys.time() -. temps_reverse_tr;;
```

## Exercice 40

Voici un type `Arbre` comportant des **entiers** à chaque noeud.
```ocaml 
type abint = Vide | Noeud of abint * int * abint;;
```
Construire (à la main) un (petit) exemple d’ABR en OCaml en utilisant ce type.

```ocaml
let arbre = Noeud(Noeud(Noeud(Vide,7,Vide),9,Vide),10,Noeud(Vide,12,Vide));;
```

## Exercice 41

Écrire la fonction `mem` qui recherche si un entier donné appartient à un ABR donné.
Il s’agit ici de profiter des caractéristiques de l’ABR pour ne pas effectuer une recherche exhaustive.

```ocaml
let rec mem arbre n = 
    match arbre with
    | Vide -> false
    | Noeud(g,x,d) -> 
        if x = n then true
        else (mem g n) || (mem d n);;
```

## Exercice 42

Écrire la fonction `insert` qui insère un entier donné dans un ABR donné, à une place
appropriée pour conserver la propriété d’ABR.
Là encore, les caractéristiques de cette structure doivent vous aider à trouver cette place facilement.

```ocaml
let rec insert arbre n =
    match arbre with
    | Vide -> Noeud(Vide,n,Vide)
    | Noeud(g,x,d) ->
        if x > n then Noeud(insert g n, x, d)
        else if x < n then Noeud(g, x, insert d n)
        else failwith "Element is already in the given BST";;
```

## Exercice 43

```ocaml
let rec verif arbre =
  match arbre with
  | Vide -> true
  | Noeud(g,x,d) -> 
    match (g,d) with
    | (Vide,Vide) -> true
    | (Noeud(gg,xg,dg),Vide) -> if xg < x then verif g else false;
    | (Vide, Noeud(gd,xd,dd)) -> if xd > x then verif d else false;
    | (Noeud(gg,xg,dg),Noeud(gd,xd,dd)) -> if xg < x && xd > x then (verif g) || (verif d) else false;;
```

## Exercice 44

TODO

```ocaml

```

## Exercice 45

```ocaml
let rec listeVersAbr l =
  match l with
  | [] -> Vide
  | x::[] -> Noeud(Vide,x,Vide)
  | x::l -> insert (listeVersAbr l) x;;

let rec triAbr arbre =
  match arbre with
  | Vide -> []
  | Noeud(Vide,x,Vide) -> x::[]
  | Noeud(g,x,Vide) -> append (triAbr g) (x::[])
  | Noeud(Vide,x,d) -> append (x::[]) (triAbr d)
  | Noeud(g,x,d) -> append (append (triAbr g) (x::[]) ) (triAbr d);;
```