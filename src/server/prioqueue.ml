(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

(* originally cut and pasted from ocaml docs
   
   it's too small to reimplement any other way if you're going to use
   an unbalanced tree
 *)

type priority = float
		
type 'a queue = 
	| Empty 
	| Node of priority * 'a * 'a queue * 'a queue

let empty = Empty
	
let rec insert queue prio elt =
	match queue with
		| Empty -> Node(prio, elt, Empty, Empty)
		| Node(p, e, left, right) ->
			  if prio <= p
			  then Node(prio, elt, insert right p e, left)
			  else Node(p, e, insert right prio elt, left)

exception Queue_is_empty
exception Comparand_too_great

let rec remove_top = function
	| Empty -> raise Queue_is_empty
	| Node(prio, elt, left, Empty) -> left
	| Node(prio, elt, Empty, right) -> right
	| Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                      (Node(rprio, relt, _, _) as right)) ->
		  if lprio <= rprio
		  then Node(lprio, lelt, remove_top left, right)
		  else Node(rprio, relt, left, remove_top right)
			  
let extract = function
	| Empty -> raise Queue_is_empty
	| Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)

let extract_if_lesser ~comparand = function
	| Empty -> raise Queue_is_empty
	| Node(prio, elt, _, _) as queue ->
		  if prio < comparand
		  then (prio, elt, remove_top queue)
		  else raise Comparand_too_great

let top_priority = function
	| Empty -> raise Queue_is_empty
	| Node(prio, _, _, _) -> prio

let rec iter ?comparand f pq =
	try
		match comparand with
			| None -> 
				  let prio, elt, pq = extract pq in
					  f prio elt;
					  iter f pq
			| Some comparand -> 
				  let prio, elt, pq = extract_if_lesser ~comparand pq in
					  f prio elt;
					  iter ~comparand f pq
	with 
		| Comparand_too_great
		| Queue_is_empty -> pq
