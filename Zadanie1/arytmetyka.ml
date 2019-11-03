(* 
    Implementacja interfejsu do zadania o arytmetyce niedokładnych wartości. 
    Autor: Filip Bienkowski
    Recenzent: Marcin Mazur
*)

(* Moduly *)

open List

(* Definicje typow *)

(*  
    Typ reprezentujacy zakres liczb rzeczywistych, oparty o rekordy. 
    Podane sa wartosc minimalna oraz maksymalna.
    W domysle 'maksimum' powinno byc zawsze wieksze lub rowne minimum. 
*)
type zakres = { minimum : float ; maksimum : float}

(*
    Typ reprezentujący niedokladna wartosc.
    Reprezentowana jest ona przez pewna ilosc zakresow.
*)
type wartosc = zakres list

(* Implementacje konstruktorow *)

(* Funkcja pomocnicza wyznaczająca 'p' procent z liczby 'x' *)
let procent (x : float) (p : float) =
    abs_float x *. (p /. 100.)

(* Zwraca wartosc rowna x +/- p% *)
let wartosc_dokladnosc (x : float) (p : float) =
    [{minimum = (x -. (procent x p)) ; maksimum = (x +. (procent x p))}]

(* Zwraca wartosc od x do y *)
let wartosc_od_do (x : float) (y : float) =
    [{minimum = x ; maksimum = y}]

(* Zwraca wartosc rowna dokladnie x *)
let wartosc_dokladna (x : float) =
    [{minimum = x ; maksimum = x}]

(* Implementacje selektorow *)

(* Sprawdza czy dany float 'x' snajduje sie w zakresie 'z' *)
let sprawdz_zakres (z : zakres) (x : float) =
    if x >= z.minimum && x <= z.maksimum then true
    else false

(* Funkcja pomocnicza dla in_wartosc. Po kolei sprawdza czy 'x' nalezy do
ktoregokolwiek z zakresow *)
let rec in_wartosc_helper (w : wartosc) (x : float) (t : bool) =
    if t then t else
    match w with
    | l :: p -> in_wartosc_helper p x (sprawdz_zakres l x)
    | [] -> false 

(* Sprawdza czy x jest wsrod mozliwych wartosci w *)
let in_wartosc (w : wartosc) (x : float) =
    in_wartosc_helper w x false

(* Funkcja pomocnicza dla min_wartosc. Po kolei sprawdza wszystkie minimalne
wartosci zakresow, i znajduje najmniejsza. Sprawdzamy wszystkie zakresy,
aby zapewnic poprawne dzialanie w przypadku nieposortowanych zakresow *)
let rec min_wartosc_helper (w : wartosc) (m : float) =
    match w with
    | l :: p -> min_wartosc_helper p (if compare m nan = 0 ||
    compare l.minimum nan = 0 then nan else min (l.minimum) m)
    | [] -> m 

(* Zwraca kres dolny mozliwych wartosci w, lub -infinity jesli nie ma ograniczenia *)
let min_wartosc (w : wartosc) =
    match w with
    | l :: p -> min_wartosc_helper w infinity
    | [] -> nan

(* Funkcja pomocnicza dla max_wartosc. Po kolei sprawdza wszystkie maksymalne
wartosci zakresow, i znajduje najwieksza. Sprawdzamy wszystkie zakresy,
aby zapewnic poprawne dzialanie w przypadku nieposortowanych zakresow *)
let rec max_wartosc_helper (w : wartosc) (m : float) =
    match w with
    | l :: p -> max_wartosc_helper p (if compare m nan = 0 ||
    compare l.maksimum nan = 0 then nan else max (l.maksimum) m)
    | [] -> m 

(* Zwraca kres gorny mozliwych wartosci w, lub infinity jesli nie ma ograniczenia *)
let max_wartosc (w : wartosc) =
    match w with
    | l :: p -> max_wartosc_helper w neg_infinity
    | [] -> nan

(* Srednia arytmetyczna max i min wartosci w *)
let sr_wartosc (w : wartosc) = 
    ((min_wartosc w) +. (max_wartosc w)) /. 2.

(* Implementacje modyfikatorow *)

(* Druga pomocnicza funkcja dla przeprowadz_dzialanie, rozbija 'a' na poszczegolne zakresy
i wykonuje zadanie dzialanie, po czym zwraca wynik *)
let rec dzialanie_helper_a (w : wartosc) (x : zakres) dzialanie (akt2 : wartosc) : wartosc =
    match w with 
    | l :: p -> dzialanie_helper_a p x dzialanie (dzialanie l x @ akt2)
    | [] -> akt2

(* Funkcja pomocnicza dla przeprowadz_dzialanie, rozbija 'b' na 
poszczegolne zakresy i przekazuje dalej *)
let rec dzialanie_helper_b (a : wartosc) (b : wartosc) dzialanie (akt : wartosc) : wartosc =
    match b with
    | l :: p -> dzialanie_helper_b a p dzialanie (dzialanie_helper_a a l dzialanie [] @ akt)
    | [] -> akt

(* Funkcja wyzszego rzedu, stanowiaca 'szkielet' dla wszystkich dzialan.
Funkcje pomocnicze rozbija wartosci na konkretne zakresy, wykonaja
zadane dzialanie, po czym zwroci wynikowa wartosc *)
let przeprowadz_dzialanie (a : wartosc) (b : wartosc) dzialanie =
    dzialanie_helper_b a b dzialanie []

(* Funkcja pomocnicza, dodaje dwa zakresy i zwraca wartosc wynikowa *)
let dodawanie (a : zakres) (b : zakres) = 
    [{minimum = a.minimum +. b.minimum ; maksimum = a.maksimum +. b.maksimum}]

(* Dodaje dwie wartosci i zwraca wartosc wynikowa *)
let plus (a : wartosc) (b : wartosc) = przeprowadz_dzialanie a b dodawanie

(* Funkcja pomocnicza, odejmuje dwa zakresy i zwraca wartosc wynikowa *)
let odejmowanie (a : zakres) (b : zakres) = 
    [{minimum = a.minimum -. b.maksimum ; maksimum = a.maksimum -. b.minimum}]

(* Odejmuje dwie wartosci i zwraca wartosc wynikowa *)
let minus (a : wartosc) (b : wartosc) = przeprowadz_dzialanie a b odejmowanie

(* Zwraca floata o najwiekszej wartosci z zadanej listy floatow *)
let maksimum_z_listy (a : float list) =
    match a with
    | l :: p -> fold_left max l p
    | [] -> nan

(* Zwraca floata o namniejszej wartosci z zadanej listy floatow*)
let minimum_z_listy (a : float list) =
    match a with
    | l :: p -> fold_left min l p
    | [] -> nan

(* Mnozy dwa floaty przez siebie, z uwzglednieniem szczegolnych przypadkow *)
let madre_mnozenie (a : float) (b : float) =
    if (a = 0. && (b = infinity || b = neg_infinity)) ||
    (b = 0. && (a = infinity || a = neg_infinity))
    then 0. else a *. b 

(* Zwraca wszystykie mozliwe sposoby pomnozenia dwoch zakresow *)
let mozliwe_mnozenia (a : zakres) (b : zakres) : float list =
    [madre_mnozenie a.minimum b.minimum; madre_mnozenie a.minimum b.maksimum;
    madre_mnozenie a.maksimum b.minimum; madre_mnozenie a.maksimum b.maksimum] 

(* Funkcja pomocnicza, mnozy dwa zakresy i zwraca wartosc wynikowa *)
let mnozenie (a : zakres) (b : zakres) : wartosc =  
    [{minimum = minimum_z_listy (mozliwe_mnozenia a b) ;
    maksimum = maksimum_z_listy (mozliwe_mnozenia a b)}]

(* Mnozy dwie wartosci i zwraca wartosc wynikowa *)
let razy (a : wartosc) (b : wartosc) = przeprowadz_dzialanie a b mnozenie

(* Wyznacza matematyczna odwrotnosc danego zakresu. Odwrotnosc musi byc
wartoscia, poniewaz moze byc wiecej niz jednym zakresem *)
let matematyczna_odwrotnosc (z : zakres) =
    match z with
    | {minimum = 0. ; maksimum = 0.} -> [{minimum = nan ; maksimum = nan}]
    | {minimum = x ; maksimum = 0.} -> [{minimum = neg_infinity ; maksimum = 1. /. x}]
    | {minimum = 0. ; maksimum = y} -> [{minimum = 1. /. y ; maksimum = infinity}]
    | {minimum = x ; maksimum = y} -> if in_wartosc [{minimum = x ; maksimum = y}] 0.
    then [{minimum = neg_infinity ; maksimum = 1. /. x} ;
    {minimum = 1. /. y ; maksimum = infinity}]
    else [{minimum = 1. /. y ; maksimum = 1. /. x}]

(* Zamienia dany zakres h w jednozakresowa wartosc *)
let zakres_w_wartosc (h : zakres) = [h]

(* Funkcja pomocnicza. Poniewaz dzielenie jest mnozeniem przez odwrotnosc,
a dzielimy przez b mnozac a przez odwrotnosc b. Wyjatkowo nie operujemy
na zakresach tylko na wartosciach, poniewaz odwrotnosc moze byc dwoma 
rozlacznymi zakresami *)
let dzielenie (a : zakres) (b : zakres) = 
    razy (zakres_w_wartosc a) (matematyczna_odwrotnosc b)

(* Dzieli dwie wartosci i zwraca wartosc wynikowa *)
let podzielic (a : wartosc) (b : wartosc) = przeprowadz_dzialanie a b dzielenie

(* Testy *)

(*
let a = in_wartosc (minus (wartosc_od_do (-2.400000) (5.200000)) (wartosc_dokladna (0.000000))) (-6.200000)
assert (a = false)
let a = sr_wartosc (razy (wartosc_dokladnosc (0.000000) (0.000000)) (wartosc_dokladnosc (-5.600000) (3.600000)))
assert (a =. 0.)
let a = in_wartosc (plus (wartosc_dokladnosc (4.200000) (6.600000)) (wartosc_od_do (0.000000) (8.400000))) (-0.800000)
assert (a = false)
let a = max_wartosc (podzielic (wartosc_dokladnosc (8.600000) (9.000000)) (wartosc_od_do (-7.400000) (-7.400000)))
assert (a =. -1.05756756756756731)
let a = in_wartosc (razy (wartosc_dokladna (-5.200000)) (wartosc_dokladnosc (-3.600000) (2.200000))) (0.000000)
assert (a = false)
let a = min_wartosc (minus (wartosc_dokladna (1.600000)) (wartosc_dokladnosc (-8.200000) (0.200000)))
assert (a =. 9.78359999999999808)
let a = min_wartosc (minus (wartosc_od_do (5.200000) (9.400000)) (wartosc_dokladna (0.800000)))
assert (a =. 4.4)
let a = sr_wartosc (podzielic (wartosc_dokladna (3.400000)) (wartosc_dokladnosc (-5.800000) (9.800000)))
assert (a =. -0.591891421766368331)
let a = min_wartosc (podzielic (wartosc_od_do (-4.200000) (9.400000)) (wartosc_dokladnosc (0.000000) (0.200000)))
assert ((classify_float a) == FP_nan)
let a = sr_wartosc (plus (wartosc_od_do (0.200000) (4.400000)) (wartosc_dokladna (1.000000)))
assert (a =. 3.30000000000000027)
*)