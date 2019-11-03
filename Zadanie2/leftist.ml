(* 
 Implementacja interfejsu kolejki priorytetowej
 Autor: Filip Bienkowski
 Recenzent: 
*)

(* Definicje typow *)

(* Typ wezla *)
type 'a node = 
    {left : 'a queue; right : 'a queue; priority : 'a; depth : int}

(* Typ kolejki *)
and 'a queue =
    | Node of 'a node
    | Leaf

(* Wyjatki *)

(* Wyjątek rzucany gdy delete_min dostaje pusta kolejke *)
exception Empty

(* Funkcje *)

(* Funkcja zwraca pusta kolejke *)
let empty = Leaf

(* Funkcja zwraca prawa wysokosc drzewa. Puste drzewo ma wysokosc -1 *)
let depth q =
    match q with
    | Leaf -> -1
    | Node({depth = d}) -> d

(* Funkcja zwraca wartosc z danego wezla (priorytet) *)
let value n = n.priority

(* Funkcja sprawdza czy kolejka jest pusta, zwraca true jesli jest *)
let is_empty q =
    match q with
    | Leaf -> true
    | Node(_) -> false 

(* 
 Funkcja porzadkuje dwa wezly wzgledem wartosci.
 W korzeniu drzewa na pewno bedzie n1, poniewaz ma wiekszy priorytet. 
*)
let order n1 n2 =
    if value n1 < value n2 then (n1, n2) else (n2, n1)

(* Funkcja rzutuje typ wezla na typ kolejki *)
let node_to_queue n = Node(n)

(* 
 Funkcja zwraca drzewo, w którego korzeniu jest korzeń n1
 natomiast poddrzewami są lewe poddrzewo n1 
 oraz drzewo n3 (czyli prawe poddrzewo n1 oraz całe drzewo n2). 
 Prawym poddrzewem zostaje to z nich, które ma mniejszą prawa wysokość.
*)
let rec new_queue_with p =
    match p with (n1, n2) ->
        let n3 = join n1.right (node_to_queue n2) in
            if depth n1.left < depth n3 
            then Node{left = n3; right = n1.left;
                      priority = n1.priority; depth = ((depth n1.left) + 1)}
            else Node{left = n1.left; right = n3;
                      priority = n1.priority; depth = ((depth n3) + 1)}

(* Funkcja laczy dwie kolejki priorytetowe q1 i q2 *)
and join q1 q2 =
    match (q1, q2) with
    | (Leaf, Leaf) -> empty
    | (Leaf, Node(_)) -> q2
    | (Node(_), Leaf) -> q1
    | (Node(n1), Node(n2)) -> new_queue_with (order n1 n2)

(* Funkcja dodaje element e do kolejki q *)
let add e q =
    join q (Node{left = Leaf; right = Leaf; priority = e; depth = 0})

(* 
 Funkcja zwraca pare z najmniejsza watroscia z kolejki 
 oraz kolejka bez tego elementu
*)
let delete_min q =
    match q with
    | Leaf -> raise Empty
    | Node({priority = p; left = l; right = r}) -> (p, (join l r))

(* Testy *)

(*
let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;
let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;
let b = join b c;;
let (a,b) = delete_min b;;
assert (a = (-5));;
let (a,b) = delete_min b;;
assert (a = (-1));;
let (a,b) = delete_min b;;
assert (a = 0);;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 2);;
let (a,b) = delete_min b;;
assert (a = 3);;
let (a,b) = delete_min b;;
assert (a = 4);;
let (a,b) = delete_min b;;
assert (a = 10);;
assert (try let _=delete_min b in false with Empty -> true);;
*)