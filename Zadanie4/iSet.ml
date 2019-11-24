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
    Recenzent:
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
}

(* Funkcje *)

(* 
    Funkcja porownujaca dwa przedzialy. 1 oznacza ze przedzial 'b' jest wiekszy,
    -1 oznacza ze przedzial 'a' jest wiekszy. 0 oznacza ze przedzialy sie przecinaja.
*)
let cmp (a1, a2) (b1, b2) =
    if b1 > a2 then 1
    else if a1 > b2 then -1
    else 0

(* Funkcja zwraca pusty zbior *)
let empty = Empty 

(* Funkcja sprawdza czy zbior jest pusty, zwraca true gdy tak, false gdy nie *)
let is_empty x = 
    match x with
    | Empty -> true
    | Node(_) -> false

(* Funkcja sprawdza czy wartosc 'x' znajduje sie w zbiorze 's' *)
let mem x s =
	let rec walk s =
		match s with
		| Node({left = l; value = v; right = r;}) ->
                if (cmp (x, x) v) = 0 then true 
                else walk (if (cmp (x, x) v) < 0 then r else l)
		| Empty -> false
	in walk s

(* Funkcja aplikuje zadana funkcje 'f' do wszystkich przedzialow w zbiorze 's' w kolejnosci rosnacej *)
let iter f s =
	let rec walk f s = 
        match s with
		| Empty -> ()
		| Node({left = l; value = v; right = r;}) -> walk f l; f v; walk f r
	in walk f s

(* 
    Funkcja liczy (f xN ... (f x2 (f x1 a))...), gdzie x1
    ... xN to wszystkie elementy zbioru 's', w kolejnosci rosnacej.
*)
let fold f s a =
	let rec walk f s a =
        match s with
		| Empty -> a
		| Node({left = l; value = v; right = r;}) -> walk f r (f v (walk f l a))
    in walk f s a

(* Zwraca liste wszystkich przedzialow zadanego zbioru 's', posortowana rosnaco *)
let elements s =
	let rec walk s a =
		match s with
		| Node({left = l; value = v; right = r;}) -> walk l (v::(walk r a))
		| Empty -> a
	in walk s []