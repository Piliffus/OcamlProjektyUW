(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz, Filip Bienkowski
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(*
    Autor: Filip Bienkowski
    Recenzent: Jakub Molinski
*)

(* Definicje typow *)

(* Typ drzewa przedzialow, oraz typ jego wezla *)
type t =
  | Empty
  | Node of node

and node = 
{
    left : t;
    right : t;
    height : int;
    value : (int * int);
    elementy_poddrzew : int
}

(* Wyjatki *)

(* Wyjatek rzucany przez funkcje szukajace skrajnych przedzialow *)
exception Brak_Skrajnego_Przedzialu

(* Wyjatek rzucany gdy funkcja dostala niepoprawny przedzial *)
exception Bledny_Przedzial

(* Funkcje pomocnicze *)

(* 
    Funkcja porownujaca dwa przedzialy. ujemna wartosc oznacza ze przedzial 'b' jest wiekszy,
    dodatnia oznacza ze przedzial 'a' jest wiekszy. 0 oznacza ze przedzialy sie przecinaja.
*)
let cmp (a1, a2) (b1, b2) =
    if b1 > a2 then -1
    else if a1 > b2 then 1
    else 0

(* Funkcja odczytująca wysokosc z zadanego wezla 'x' *)
let wysokosc =
    function
    | Node({height = h}) -> h 
    | Empty -> 0

(* zwraca liczbę elementów w obu poddrzewach *)
let elementy_poddrzewa =
    function
    | Node ({elementy_poddrzew = e}) -> e
    | Empty -> 0

(* Zwraca sumę liczb lub max_int jeśli przekracza ona max_int *)
let suma a b c d =
    if max (max a b) (max c d) = 
        max_int ||
        a >= max_int - b ||
        c >= max_int - d ||
        a + b >= max_int - c - d
    then max_int
    else a + b + c + d

(* Wielkosc przedziału w wezle *)
let ile_elementow =
    function
    | Node ({value = (v1, v2)}) -> abs (v2 - v1) + 1
    | Empty -> 0

(* Konstruktor wezla o poddrzewach 'l', 'r' i przedziale 'v' *)
let make l v r =
    Node({left = l; value = v; right = r; 
          height = max (wysokosc l) ((wysokosc r) + 1); 
          elementy_poddrzew = suma 
          (elementy_poddrzewa l) (ile_elementow l) (elementy_poddrzewa r) (ile_elementow r)})

(* 
    Balansuje drzewo utworzone z poddrzew 'l' i 'r' oraz zakresu 'v'
    tak, by maksymalna różnica wysokości poddrzew nie była większa od 2.
    Zakładamy, że 'l' i 'r' są zrównoważone względem siebie
*)
let bal l v r =
    let hl = wysokosc l in
    let hr = wysokosc r in
    if hl > hr + 2 then
        match l with
        | Node({left = ll; value = lv; right = lr;}) ->
                if wysokosc ll >= wysokosc lr then make ll lv (make lr v r)
                else (match lr with
                | Node({left = lrl; value = lrv; right = lrr;}) ->
                        make (make ll lv lrl) lrv (make lrr v r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
        match r with
        | Node({left = rl; value = rv; right = rr;}) ->
                if wysokosc rr >= wysokosc rl then make (make l v rl) rv rr
                else (match rl with
                | Node({left = rll; value = rlv; right = rlr;}) ->
                        make (make l v rll) rlv (make rlr rv rr)
                | Empty -> assert false)
        | Empty -> assert false
    else make l v r

(* dodaje przedział 'x' do drzewa 's' zakładając, że jest rozłączny z wszystkimi przedziałami *)
let rec add_one x =
    function
    | Node({left = l; value = v; right = r; height = h}) ->
            let c = cmp x v in
            if c = 0 then make l x r 
            else if c < 0 
                then
                bal (add_one x l) v r
                else
                bal l v (add_one x r)
    | Empty -> make Empty x Empty

(* Zwraca drzewo zawierające elementy z 'l' 'r' i wezel z przedzialem 'v' *)
let rec join l v r =
    match (l, r) with
    | (Empty, _) -> add_one v r
    | (_, Empty) -> add_one v l
    | Node({left = ll; value = lv; right = lr; height = lh}), 
      Node({left = rl; value = rv; right = rr; height = rh}) ->
            if lh > rh + 2 then bal ll lv (join lr v r) else
            if rh > lh + 2 then bal (join l v rl) rv rr else 
            make l v r

(* 
    Zwraca drzewo powstałe po złączeniu 's1' z 's2' w taki sposób, że
    w węźle nowo powstałego drzewa jest minimalny element  drzewa 's2'
*)
let rec sklej s1 s2 =
    let rec min_element =
        function
        | Node({left = Empty; value = v}) -> v
        | Node({left = l}) -> min_element l
        | Empty -> raise Not_found
    in
    let rec usun_min_element =
        function
        | Node({left = Empty; right = r}) -> r
        | Node({left = l; value = v; right = r}) -> bal (usun_min_element l) v r
        | Empty -> invalid_arg "blad"
    in
    match s1, s2 with
    | Empty, _ -> s2
    | _, Empty -> s1
    | _ -> 
    let r = min_element s2 in
    let s3 = usun_min_element s2 in
    join s1 r s3

(* 
    Znajduje w 's' skrajnie lewy i prawy przedział dla
    przedziału 'v' i zwraca przedział będący sumą przedziału skrajnie lewego
    i prawego, o ile w ogóle istnieją. Funkcja pomicnicza zwraca skrajnie lewy/prawy
    przedzial nachodzacy albo zazebiajacy sie z 'v'. Jezeli nie ma takiego to
    rzuca wyjatek.
*)
let znajdz_skrajne s v =
    let rec znajdz_lewy_skrajny s (q1, q2) = 
    match s with
    | Empty -> raise Brak_Skrajnego_Przedzialu
    | Node({left = l; value = (v1, v2); right = r}) ->
        if (v1 <= q1 && q1 <= v2) || (v2 = q1 - 1 && q1 != min_int) then (v1, v2) 
        else if v1 > q1 then
            try znajdz_lewy_skrajny l (q1, q2) with
                | Brak_Skrajnego_Przedzialu ->
                    if v1 <= q2 then (v1, v2)
                    else raise Brak_Skrajnego_Przedzialu
        else znajdz_lewy_skrajny r (q1, q2)
    in
    let rec znajdz_prawy_skrajny s (q1, q2) = 
    match s with
    | Empty -> raise Brak_Skrajnego_Przedzialu
    | Node({left = l; value = (v1, v2); right = r}) ->
        if (v1 <= q2 && q2 <= v2) || (v1 = q2 + 1 && q2 != max_int) then (v1, v2) 
        else if v2 < q2 then
            try znajdz_prawy_skrajny r (q1, q2) with
                | Brak_Skrajnego_Przedzialu ->
                    if q1 <= v2 then (v1, v2)
                    else raise Brak_Skrajnego_Przedzialu
        else znajdz_prawy_skrajny l (q1, q2) 
    in
    let v1 =
        try znajdz_lewy_skrajny s v with
        | Brak_Skrajnego_Przedzialu -> v
    and v2 =
        try znajdz_prawy_skrajny s v with
        | Brak_Skrajnego_Przedzialu -> v
    in 
    (v1, v2)

(* 
    Zwraca krotke (l, present, r), gdzie 'l' to drzewo zawierające 
    przedziały mniejsze od 'x' a 'r' to przedziały większe od 'x'.
    Present to wartość logiczna oznaczająca istnienie 'x' w 's'
*)
let split_pset x s =
    let rec walk x =
        function
        | Empty -> (Empty, false, Empty)
        | Node({left = l; value = v; right = r;}) ->
            let c = cmp x v in
            if c = 0 then (l, true, r) else
                if c < 0 then
                let (ll, pres, rl) = walk x l
                in (ll, pres, join rl v r)
                else
                let (lr, pres, rr) = walk x r
                in (join l v lr, pres, rr)
    in 
    walk x s

(* Funkcje z interfejsu *)

(* Funkcja zwraca pusty zbior *)
let empty = Empty 

(* Funkcja sprawdza czy zbior jest pusty, zwraca 'true' gdy tak, 'false' gdy nie *)
let is_empty s = 
    match s with
    | Empty -> true
    | Node(_) -> false

(* Sprawdza, czy liczba całkowita 'x' należy do 's' *)
let mem x s =
    let rec walk =
        function
        | Node({left = l; value = (a1, a2); right = r;}) ->
                (a1 <= x && x <= a2) || walk (if x < a1 then l else r)
        | Empty -> false
    in 
    walk s

(* Funkcja aplikuje zadana funkcje 'f' do wszystkich przedzialow w 's' w kolejnosci rosnacej *)
let iter f s =
    let rec walk =
        function
        | Empty -> ()
        | Node({left = l; value = v; right = r;}) ->
                walk l; f v; walk r
    in 
    walk s

(* 
    Funkcja liczy (f xN ... (f x2 (f x1 a))...), gdzie x1
    ... xN to wszystkie elementy zbioru 's', w kolejnosci rosnacej.
*)
let fold f s a =
    let rec walk a =
        function
        | Empty -> a
        | Node({left = l; value = v; right = r;}) ->
                walk (f v (walk a l)) r
    in 
    walk a s

(* Zwraca liste wszystkich przedzialow zadanego zbioru 's', posortowana rosnaco *)
let elements s =
    let rec walk a =
        function
        | Empty -> a
        | Node({left = l; value = v; right = r;}) ->
                walk (v :: walk a r) l
    in 
    walk [] s

(* 
    Zwraca liczbę elementow w drzewie mniejszych/rownych x.
    Przechodząc po drzewie kumulujemy wynik, jeśli dojdziemy do przedzialu
    zawierającego x, to zwracamy wynik. W przeciwnym razie
    dodajemy liczebnosc nastepnych elementow mniejszych/rownych x 
*)
let below x s =
    let rec walk s =
        match s with
        | Node({left = l; value = (v1, v2); right = r;}) ->
                let c = cmp (x, x) (v1, v2) in
                if c = 0 then
                    if x - v1 + 1 <= 0 then max_int
                    else suma (elementy_poddrzewa l) (ile_elementow l) (x - v1 + 1) 0
                else
                    if c < 0 then walk l
                    else suma (elementy_poddrzewa l) (ile_elementow l) (ile_elementow s) (walk r)
        | Empty -> 0
    in 
    walk s

(*
    Zwraca drzewo zawierające wszystkie przedziały z 's' oraz przedzial 'x'.
    Szukamy skrajnie lewego i prawego przedziału dla przedziału
    'x', a następnie znajdujemy przedziały
    mniejsze i większe od 'x2', czyli przedziałem zrobionego z 'x'
    oraz skrajnie lewego i prawego przedziału. Na koniec tworzymy nowe drzewo
    z przedziału 'x2' i przedzialow mniejszych i wiekszych
*)
let add x s =
    if fst x > snd x then raise Bledny_Przedzial else
        let (l', r') = znajdz_skrajne s x in
        if l' = x && r' = x then add_one x s else
        let (l_tree, _, r1_tree) = split_pset l' s in
        let (_, _, r_tree) = split_pset r' s in
        let x2 = (min (fst l') (fst x), max (snd r') (snd x)) in
        join l_tree x2 r_tree

(* 
    Zwraca krotke ('l', 'present', 'r'), gdzie 'l' to
    drzewo z przedziałami < 'x' a 'r' > x. 'present' to wartość logiczna 
    oznaczająca czy 'x' jest w 's'.
    Szukamy skrajnie lewego i prawego przedziału w drzewie dla (x, x), a następnie 
    znajdujemy przedziały mniejsze i większe od (y, y), czyli przedziału
    zrobionego z (x, x) oraz skrajnie lewego i prawego przedziału. Następnie
    dodajemy elementy ze skrajnych przedziałów mniejsze/większe od 'x' do drzew
    'l' i 'r'.
*)
let split x s =
    let (lewy_skraj, prawy_skraj) = znajdz_skrajne s (x, x) in
    let (l1, _, r1) = split_pset lewy_skraj s in
    let (_, _, r2) = split_pset prawy_skraj r1 in
    let l2 =
        if fst lewy_skraj < x
        then add (fst lewy_skraj, x - 1) l1
        else l1
    and r3 =
        if snd prawy_skraj > x
        then add (x + 1, snd prawy_skraj) r2
        else r2
    in 
    (l2, mem x s, r3)

(* 
    Usuwa z drzewa przedzial 'x' i zwraca nowo powstałe
    drzewo. Szukamy skrajnie lewego i prawego przedziału w dla
    'x', a potem znajdujemy przedziały
    mniejsze i większe od y, gdzie y jest przedziałem utworzonym z 'x'
    i skrajnie lewego i prawego przedziału. Następnie laczymy je dodajac
    do nich skrajnie lewy i prawy przedział bez czesci wspplnej z 'x' 
*)
let remove x s =
  if fst x > snd x then raise Bledny_Przedzial else
    let (lewy_skraj, prawy_skraj) = znajdz_skrajne s x in
    let (l1, _, r1) = split_pset lewy_skraj s in
    let (_, _, r2) = split_pset prawy_skraj r1 in
    let nowe_s = sklej l1 r2 in
    let nowe_s_2 =
        if fst lewy_skraj < fst x
        then add (fst lewy_skraj, fst x - 1) nowe_s
        else nowe_s
    in
    if snd prawy_skraj > snd x
    then add (snd x + 1, snd prawy_skraj) nowe_s_2
    else nowe_s_2