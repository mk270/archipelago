(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

let filter_by_label label nodes =
	List.filter (fun (l, _) -> l == label) nodes

let remove label needle haystack = 
	List.filter (fun (l, i) -> l != label || i != needle) haystack

(* List.memq for a list of tuples *)
let is_snd_member label needle haystack =
	let rec is_snd_member' = function
		| [] -> false
		| hd :: tl -> 
			if label == fst hd && needle == snd hd
			then true
			else is_snd_member' tl
	in
		is_snd_member' haystack
	
type structure =
	| Graph
	| Tree

let outdegree_unlimited = function
	| Graph -> true
	| Tree -> false

type ('a, 'b) node = {
	thing: 'a;
	mutable inbound: ('b * ('a, 'b) node) list;
	mutable outbound: ('b * ('a, 'b) node) list;
	fn : 'b -> structure;
}

let create f thing = {
	thing = thing;
	inbound = [];
	outbound = [];
	fn = f;
}

let receive destination source label =
  destination.outbound <- (label, source) :: destination.outbound

let eject destination source label =
  assert (is_snd_member label source destination.outbound);

  destination.outbound <- remove label source destination.outbound

let remove_from destination source label =
  eject destination source label;
  source.inbound <- remove label destination source.inbound

let insert_into destination source label =
	let graph_structure = destination.fn label in
	let outdegree_unltd = outdegree_unlimited graph_structure in
		if (not outdegree_unltd)
		then ( 
			match (filter_by_label label source.inbound) with
			| [] -> ()
			| [hd] -> remove_from (snd hd) source label
			| _ -> assert false
		);

		receive destination source label;
		source.inbound <- (label, destination) :: source.inbound

let sources_of destination label =
	List.map snd (filter_by_label label destination.outbound)

let contained i =
	i.thing
	  
let destinations_of source label =
	List.map snd (filter_by_label label source.inbound)
