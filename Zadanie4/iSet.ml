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

(* 
    Typ drzewa przedzialow, oraz typ jego wezla: 
    lewy syn, wartosc, prawy syn, wysokosc, liczba elementow w poddrzewach.
    Wysokosc >=1, liczba el. >= 0, wartosc to zakres (int, int).
*)
type t =
    | Node of t * (int * int) * t * int * int
    | Empty

(* Wyjatki *)

(* Wyjatek rzucany przez funkcje szukajace skrajnych przedzialow *)
exception Brak_Skrajnego_Przedzialu

(* Wyjatek rzucany gdy funkcja dostala niepoprawny przedzial *)
exception Bledny_Przedzial

(* Funkcje pomocnicze *)

(* 
    Funkcja porownujaca dwa przedzialy. ujemna wartosc oznacza ze przedzial 'b' 
    zaczyna sie od wiekszych wartosci, dodatnia oznacza ze przedzial 'a'. 
    0 oznacza ze przedzialy sie przecinaja.
*)
let cmp (a1, a2) (b1, b2) = 
    if a2 < b1 then -1 else
    if b2 < a1 then 1 else 0

(* Funkcja odczytująca wysokosc z zadanego wezla *)
let wysokosc =
  function
    | Node(_, _, _, h, _) -> h
    | Empty -> 0

(* zwraca liczbę elementów w obu poddrzewach *)
let elementy_poddrzew =
    function
    | Node(_, _, _, _, e) -> e
    | Empty -> 0

(* Wielkosc przedziału w wezle *)
let rozmiar_przedzialu =
    function
    | Node(_, (v1, v2), _, _, _) -> 
      let cand = abs (v2 - v1) + 1 in
      if v2 > 0 && v1 > 0 then cand else
      if (v2-v1) < 0 then max_int else cand
    | Empty -> 0

(* Zwraca sumę liczb lub max_int jeśli przekracza ona max_int *)
let suma a b =
    if a + b < 0
    then max_int
    else a + b

(* Suma listy intow *)
let suma_list l =
    List.fold_left suma 0 l

(* Konstruktor wezla o poddrzewach 'l', 'r' i przedziale 'v' *)
let make l v r =
    Node(l, v, r, max (wysokosc l) (wysokosc r) + 1, 
    suma_list [(elementy_poddrzew l); (rozmiar_przedzialu l); (elementy_poddrzew r); (rozmiar_przedzialu r)])

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
        | Node(ll, lv, lr, _, _) ->
            if wysokosc ll >= wysokosc lr then make ll lv (make lr v r)
            else
            (match lr with
            | Node(lrl, lrv, lrr, _, _) ->
                make (make ll lv lrl) lrv (make lrr v r)
            | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
        match r with
        | Node(rl, rv, rr, _, _) ->
            if wysokosc rr >= wysokosc rl then make (make l v rl) rv rr
            else
            (match rl with
            | Node(rll, rlv, rlr, _, _) ->
                make (make l v rll) rlv (make rlr rv rr)
            | Empty -> assert false)
        | Empty -> assert false
    else make l v r

(* 
    Znajduje w 's' skrajnie lewy i prawy przedział dla
    przedziału 'v' i zwraca przedział będący sumą przedziału skrajnie lewego
    i prawego, o ile w ogóle istnieją. Funkcja pomicnicza zwraca skrajnie lewy/prawy
    przedzial nachodzacy albo zazebiajacy sie z 'v'. Jezeli nie ma takiego to
    rzuca wyjatek.
*)
let znajdz_skrajne s v =
    let rec znajdz_lewy_skrajny s (a1, a2) = 
        match s with
        | Empty -> raise Brak_Skrajnego_Przedzialu
        | Node(l, (v1, v2), r, _, _) ->
        if (v1 <= a1 && a1 <= v2) || (v2 = a1 - 1 && a1 != min_int) then (v1, v2) else
            if v1 > a1 then
            try znajdz_lewy_skrajny l (a1, a2) with
            | Brak_Skrajnego_Przedzialu ->
                if v1 <= a2 then (v1, v2)
                else raise Brak_Skrajnego_Przedzialu
            else znajdz_lewy_skrajny r (a1, a2) 
    in
    let rec znajdz_prawy_skrajny s (a1, a2) = 
        match s with
        | Empty -> raise Brak_Skrajnego_Przedzialu
        | Node(l, (v1, v2), r, _, _) ->
        if (v1 <= a2 && a2 <= v2) || (v1 = a2 + 1 && a2 != max_int) then (v1, v2) else
            if v2 < a2 then
            try znajdz_prawy_skrajny r (a1, a2) with
            | Brak_Skrajnego_Przedzialu ->
                if a1 <= v2 then (v1, v2)
                else raise Brak_Skrajnego_Przedzialu
            else znajdz_prawy_skrajny l (a1, a2) 
    in
    let lewy_skrajny =
    try znajdz_lewy_skrajny s v with
    | Brak_Skrajnego_Przedzialu -> v
    in
    let prawy_skrajny =
    try znajdz_prawy_skrajny s v with
    | Brak_Skrajnego_Przedzialu -> v
    in 
    (lewy_skrajny, prawy_skrajny)

(* dodaje przedział 'x' do drzewa 's' zakładając, że jest rozłączny z wszystkimi przedziałami *)
let rec add_one x =
    function
    | Node(l, v, r, h, _) ->
        let c = cmp x v in
        if c = 0 then make l x r 
        else if c < 0 then
            bal (add_one x l) v r
        else
            bal l v (add_one x r)
    | Empty -> make Empty x Empty

(* Zwraca drzewo zawierające elementy z 'l' 'r' i wezel z przedzialem 'v' *)
let rec join l v r =
    match (l, r) with
    | (Empty, _) -> add_one v r
    | (_, Empty) -> add_one v l
    | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
            if rh > lh + 2 then bal (join l v rl) rv rr else 
                make l v r

(* 
    Zwraca krotke (l, present, r), gdzie 'l' to drzewo zawierające 
    przedziały mniejsze od 'x' a 'r' to przedziały większe od 'x'.
    Present to wartość logiczna oznaczająca istnienie 'x' w 's'
*)
let rozdziel x s =
    let rec walk x =
        function
        | Empty -> (Empty, false, Empty)
        | Node(l, v, r, _, _) ->
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

(* 
    Zwraca drzewo powstałe po złączeniu 's1' z 's2' w taki sposób, że
    w węźle nowo powstałego drzewa jest minimalny element  drzewa 's2'
*)
let rec sklej s1 s2 = 
    let rec min_element =
        function
        | Node(Empty, v, _, _, _) -> v
        | Node(l, _, _, _, _) -> min_element l
        | Empty -> raise Not_found
    in
    let rec usun_min_element =
        function
        | Node(Empty, _, r, _, _) -> r
        | Node(l, v, r, _, _) -> bal (usun_min_element l) v r
        | Empty -> invalid_arg "blad"
    in
    match s1, s2 with
    | Empty, _ -> s2
    | _, Empty -> s1
    | _ -> 
        let r = min_element s2 in
        let s3 = usun_min_element s2 in 
        join s1 r s3

(* Funkcje z interfejsu *)

(* Funkcja zwraca pusty zbior *)
let empty = Empty

(* Funkcja sprawdza czy zbior jest pusty, zwraca 'true' gdy tak, 'false' gdy nie *)
let is_empty = 
    function
    | Empty -> true
    | Node(_) -> false

(* Sprawdza, czy liczba całkowita 'x' należy do 's' *)
let mem x s =
    let rec walk =
        function
        | Node(l, (v1, v2), r, _, _) ->
            (v1 <= x && x <= v2) || walk (if x < v1 then l else r)
        | Empty -> false
    in 
    walk s

(* Funkcja aplikuje zadana funkcje 'f' do wszystkich przedzialow w 's' w kolejnosci rosnacej *)
let iter f s =
    let rec walk =
        function
        | Empty -> ()
        | Node(l, v, r, _, _) ->
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
        | Node(l, k, r, _, _) ->
            walk (f k (walk a l)) r
    in 
    walk a s

(* Zwraca liste wszystkich przedzialow zadanego zbioru 's', posortowana rosnaco *)
let elements s =
    let rec walk a =
        function
        | Empty -> a
        | Node(l, v, r, _, _) ->
            walk (v :: walk a r) l
    in 
    walk [] s

(*
    Zwraca drzewo zawierające wszystkie przedziały z 's' oraz przedzial 'x'.
    Szukamy skrajnie lewego i prawego przedziału dla przedziału
    'x', a następnie znajdujemy przedziały
    mniejsze i większe od 'x2', czyli przedziałem zrobionego z 'x'
    oraz skrajnie lewego i prawego przedziału. Na koniec tworzymy nowe drzewo
    z przedziału 'x2' i przedzialow mniejszych i wiekszych
*)
let add x s =
    if fst x > snd x then raise Bledny_Przedzial 
    else let (lewy_skrajny, prawy_skrajny) = znajdz_skrajne s x in
        if lewy_skrajny = x && prawy_skrajny = x then add_one x s else
        let (l1, _, r1) = rozdziel lewy_skrajny s in
        let (_, _, r2) = rozdziel prawy_skrajny s in
        let x2 = (min (fst lewy_skrajny) (fst x), max (snd prawy_skrajny) (snd x)) in
        join l1 x2 r2

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
    let (lewe_skrajne, prawe_skrajne) = znajdz_skrajne s x in
    let (l1, _, r1) = rozdziel lewe_skrajne s in
    let (_, _, r2) = rozdziel prawe_skrajne r1 in
    let nowe_s = sklej l1 r2 in
    let nowe_s2 =
        if fst lewe_skrajne < fst x
        then add (fst lewe_skrajne, fst x - 1) nowe_s
        else nowe_s
    in
    if snd prawe_skrajne > snd x
    then add (snd x + 1, snd prawe_skrajne) nowe_s2
    else nowe_s2

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
    let (l1, _, r1) = rozdziel lewy_skraj s in
    let (_, _, r2) = rozdziel prawy_skraj r1 in
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
    Zwraca liczbę elementow w drzewie mniejszych/rownych x.
    Przechodząc po drzewie kumulujemy wynik, jeśli dojdziemy do przedzialu
    zawierającego x, to zwracamy wynik. W przeciwnym razie
    dodajemy liczebnosc nastepnych elementow mniejszych/rownych x 
*)
let below x s =
    let rec walk s =
    match s with
    | Node(l, (v1, v2), r, _, _) ->
        let c = cmp (x, x) (v1, v2) in
        if c = 0 then
            if x - v1 + 1 <= 0 then max_int
            else suma_list [(elementy_poddrzew l); (rozmiar_przedzialu l); (x - v1 + 1); 0]
        else
            if c < 0 then walk l
            else suma_list [(elementy_poddrzew l); (rozmiar_przedzialu l); (rozmiar_przedzialu s); (walk r)]
    | Empty -> 0
  in 
  walk s

(* Testy *)
(*
    let a = add (0, 5) empty;;
    let a = add (7, 8) a;;
    let a = add (-3, -3) a;;
    let a = add (10, 13) a;;
    assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
    assert(below 8 a = 9);;
    let b = add (6, 6) a;;
    let b = remove (6, 6) b;;
    let b = add (-100, -5) b;;
    let b = add (-4, 6) b;;
    assert(elements b = [(-100, 8); (10, 13)]);;
    assert(below 10 b = 110);;
    let c = remove (2, 10) a;;
    assert(elements c = [(-3, -3); (0, 1); (11, 13)]);;
    assert(below 12 c = 5);;
*)