(*
    Autor: Filip Bienkowski
    Recenzent: Jeremi Gladkowski
*)

(* Moduly *)

open List

(* Definicje typow *)

(* Typ punktu na plaszczyznie. Plaszczyzna jest dwuwymiarowa *)
type point = 
    float * float

(* Typ ktory udziela informacji ile przebije szpilka wbita w danym punkcie *)
and kartka = 
    point -> int

(* Wyliczeniuowy typ reprezentujący polozenie punktu względem prostej zgiecia *)
type side = 
    | Left
    | Right
    | Middle

(* Definicje funkcji *)

(* 
    Zwraca kartke (typ), reprezentujaca domknięty
    prostokąt o bokach rownoleglych do osi układu wspolrzędnych i lewym
    dolnym rogu p1 i prawym gornym p2. Punkt p1 musi wiec byc
    nieostro na lewo i w dol od punktu p2. Gdy w kartkę te wbije sie 
    szpilkę wewnatrz (lub na krawedziach) prostokata, kartka zostanie
    przebita 1 raz, w pozostalych przypadkach 0 razy 
*)
let prostokat p1 p2 = 
    let (lewo, dol) = p1 in
    let (prawo, gora) = p2 in
    function (x, y) ->
	    if (lewo <= x) && (x <= prawo) && (dol <= y) && (y <= gora) 
        then 1
        else 0

(* 
    Zwraca kartke (typ), reprezentujaca kolko domkniete o srodku
    w punkcie p i promieniu r 
*)
let kolko p r =
    let (xp, yp) = p in
    let druga_potega a = a *. a in
    function (x, y) ->
	    if druga_potega (x -. xp) +. druga_potega (y -. yp) <= druga_potega r 
        then 1 
        else 0

(* 
    Sklada kartke k wzdluz prostej przechodzacej
    przez punkty p1 i p2 (musza to byc rozne punkty). Papier jest
    skladany w ten sposob, ze z prawej strony prostej (patrzac w kierunku
    od p1 do p2) jest przekladany na lewa. Wynikiem funkcji jest
    zlozona kartka. Jej przebicie po prawej stronie prostej powinno wiec
    zwrocic 0. Przebicie dokladnie na prostej powinno zwrocic tyle samo,
    co przebicie kartki przed zlozeniem. Po stronie lewej - tyle co przed
    zlozeniem plus przebicie rozlozonej kartki w punkcie, ktory nalozyl
    sie na punkt przebicia. 
*)
let zloz p1 p2 k =
    let (x1, y1) = p1 in
    let (x2, y2) = p2 in
    (* Szuka jak wzgledem prostej polozony jest punkt *)
    let ktora_strona =
	(* Prosta rownolegla do osi Y *)
	if x1 = x2 then
	    let (mniej, wiecej) = if y2 < y1 then (Right, Left) else (Left, Right) in
        function (x, _) -> 
            if x < x1 then mniej else if x = x1 then Middle else wiecej
	(* Sprawdza czy punkt jest nad/pod prosta *)
	else 
        function (x, y) -> 
		    let w = (y1 -. y2) *. (x -. x2) +. (x1 -. x2) *. (y2 -. y) in
            if w < 0. then Right else if w = 0. then Middle else Left
    (* Znajduje wspolrzedne punktu symetrycznego do danego punktu wzgledem prostej *)
    and punkt_symetryczny = 
        function (x, y) ->
	        (* Proste równoległe do osi X / Y *)
	        if x1 = x2 then
                (2. *. x1 -. x, y)
	        else if y1 = y2 then
                (x, 2. *. y1 -. y)
	        else
                (* Wspolczynniki prostej i prostej prostopadlej *)
	            let a = (y1 -. y2) /. (x1 -. x2)
	            and a_prostopadle = (x2 -. x1) /. (y1 -. y2) in
		        (* Wyrazy wolne prostej i prostej przechodzącej przez podany punkt *)
		        let b = y2 -. a *. x2
		        and b_przechodzacej = y -. a_prostopadle *. x in
		        (* Przeciecie *)
		        let xc = (b_przechodzacej -. b) /. (a -. a_prostopadle) in
		        let yc = a *. xc +. b in
			    (* Wspolrzedne punktu symetrycznego *)
			    (2. *. xc -. x, 2. *. yc -. y)
    in 
    function p -> 
        match ktora_strona p with
	    | Right -> 0
	    | Middle -> k p
	    | Left -> k p + k (punkt_symetryczny p)

(* Zlozenie kartki k kolejno wzdluz wszystkich prostych z listy *)
let skladaj l k = 
    fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l 
