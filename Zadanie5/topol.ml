(*
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Filip Bienkowski
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
    Recenzent: Bohdan Petraszczuk
*)

(* Moduly *)

open PMap

(* Wyjatki *)

(* wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(* Funkcje pomocnicze *)

(* [new_map g] funkcja, która dla zadanej listy tworzy mapę taką, że
wartością każdego wierzchołka jest para [(flaga, lista)], gdzie [flaga]
to stan odwiedzenia (0, 1 lub 2), a [lista] to lista sąsiadujących
wierzchołków *)
let new_map g =
    List.fold_left
        (fun a (v, l) ->
        add v (0, l) a)
    empty g

(* [visited v g] zwraca 0 w przypadku gdy wierzchołek nie został jeszcze
odwiedzony, 1 - gdy jest w trakcie operacji odwiedzania i 2 - gdy został
już odwiedzony*)
let visited v g =
    match (find v g) with
    | (a, _) -> a

(* Funkcje z interfejsu *)

(* 
    Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il 
*)
let topol list =
    let result = 
        ref []
    and g = 
        ref (new_map list) 
    in
    let rec dfs v =
        if mem v (!g) 
        then
        let lk = 
            snd (find v (!g)) 
        in
        match visited v (!g) with
        | 2 -> ()
        | 1 -> raise Cykliczne
        | 0 ->
            g := add v (1, lk) (!g);
            List.iter dfs lk;
            result := (v :: (!result));
            g := add v (2, lk) (!g)
        | _ -> assert false
        else
        begin
            g := add v (2, []) (!g);
            result := (v :: (!result))
        end
    in
    let rec walk lista_g =
        match lista_g with
        | [] -> ()
        | (v, l) :: t ->
            dfs v;
            walk t
    in
    walk list;
    !result

(* Testy *)
(*
(* Autor: Marek Puzyna
 * Licence: Unlicensed
 * Original repo: https://github.com/Day1721/UW *)
 
let czy_cykliczne l =
   match (try (topol l) with
      Cykliczne -> []) with
         | [] -> true
         | _ -> false
let test input output =
   let rec loop a b f = function
      | [] -> false
      | h::t -> 
         if f then 
            if h = b then true 
            else loop a b f t
         else if h = a then loop a b true t 
            else loop a b f t
   and pom i a = function
      | [] -> (match i with
         | [] -> true
         | g::o -> pom o (fst g) (snd g))
      | h::t -> match (loop a h false output) with
         | true -> pom i a t
         | false -> false in
   pom (List.tl input) (fst (List.hd input)) (snd (List.hd input))
let a = [(1, [2]); (2, [3]); (3, [4]); (4, [1])]
let b = [(1, [2]); (2, [3]); (3, [4])]
let c = [('A', ['B'; 'C'; 'E']); ('D', ['F'; 'E'; 'G']); ('B', ['C'; 'D']);
   ('C', ['D'; 'F']); ('F', ['G'; 'H'])]
let d = [("zolty", ["niebieski"; "bialy"; "czarny"]); ("bialy", ["czarny"]); 
   ("czarny", []); ("czerwony", ["zielony"; "zolty"; "niebieski"; "czarny"])]
let e = [(1, [2; 5; 8; 3]); (5, [8; 6; 4; 7]); (7, [6; 9; 2]); (8, [6; 9; 3])]
let _ = assert(czy_cykliczne a);
        assert(not (czy_cykliczne b));
        assert(test b (topol b));
        assert(test c (topol c));
        assert(test (List.tl c) (topol (List.tl c)));
        assert(test d (topol d));
        assert(test e (topol e));
        assert(test (List.tl e) (topol (List.tl e)));
        assert(test (b @ e) (topol (b @ e)));
        assert(test (List.tl b @ e) (topol (List.tl b @ e)))
*)