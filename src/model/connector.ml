(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Objreference

type connv =
	| VTree
	| VDigraph

type 'a connector = {
	thing : 'a;

	mutable parents: (objreference * 'a connector option) list;
	mutable children: (objreference * 'a connector list) list;

	mutable successors: (objreference * 'a connector list) list;
	mutable predecessors: (objreference * 'a connector list) list;
}

let connv_of_objreference = function
	| Containment
    | Key 
    | Vulnerability
    | BodyPossesses
    | BodyWears 
    | LinkToRoom _
    | LinkPortal _
    | LinkRestriction _ -> VTree
    | CombatOpponent
    | IsBody
    | Drive -> assert false


	let create thing =
		{
			thing = thing;
			parents = [];
			children = [];
			successors = [];
			predecessors = [];
		}

	let receive network_id parent child =		
		
		parent.children <- child :: parent.children

	let eject network_id parent child =
		( match child.parent with
			| None -> failwith "Attempt to eject child with no parent from a parent"
			| Some p -> assert (List.memq child parent.children) );

		parent.children <- Utils.remove child parent.children
			
	let remove_from network_id parent child =
		eject parent child;
		child.parent <- None
			
	let insert_into network_id parent child =
		( match child.parent with
			| None -> ()
			| Some oldParent -> remove_from oldParent child );

		receive parent child;
		child.parent <- Some parent

	let children network_id parent =
		parent.children

	let parent network_id child =
		child.parent


let add_arc network_id (src : 'a connector) (dst : 'a connector) =
	let connection_type = connv_of_objreference network_id in
		match connection_type with
			| VDigraph -> assert false
			| VTree -> 
				(if not (List.mem_assq network_id src.trees)
				then let new_conn = IntContainer.create () in
						 src.trees <- (network_id, new_conn) :: src.trees);
				let src_cont = List.assq network_id src.trees in
					IntContainer.insert_into dst src_cont
				
