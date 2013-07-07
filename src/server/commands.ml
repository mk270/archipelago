(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Direction
open Verbs
open Utils

open Parser
open Verb_list

let verbs = Hashtbl.create 50

let help ~actor =
	Session.emitl actor "There is not much help available at the moment.";
	Session.emitl actor "";
	Session.emitl actor "Available verbs:";
	Hashtbl.iter (fun k v -> Session.emitl actor v.v_name) verbs


let verb_tbl = ref []
(*
let find_verb cmd =
	Hashtbl.find verbs cmd
*)

let find_verb cmd =
	let results = List.filter (fun i -> initial_match cmd i.v_name) !verb_tbl in
		match results with
			| [] -> raise Not_found
			| hd :: tl -> hd

let handle_words ~actor words line =
	let cmd = List.hd words in
	let verb = 
		try find_verb cmd
		with Not_found -> unknown_verb
	in
		try
			Parser.exec verb ~actor line
		with e -> guard_exceptions ~actor e


let parse_line p line =
  let words = Str.split delim_ws line in
	  if not (0 = List.length words)
	  then handle_words ~actor:p words line 

let init () =
	let dir_aliases = [
		("ne", "northeast");
		("se", "southeast");
		("sw", "southwest");
		("nw", "northwest");
	] in

	let compass = Direction.get_compass () in
	let dir_cmds = List.map (fun dir ->
		{ v_name = string_of_direction dir;
		  v_frame = Unary (go_dir dir);
		  v_args = []	; }
	) compass in
	let aliases = List.map (fun dir ->
		{ v_name = fst dir;
		  v_frame = Unary (go_dir (Direction.match_direction (snd dir)));
		  v_args = []; } 
	) dir_aliases
	in
		verb_tbl := dir_cmds @ aliases @ vv;
		List.iter (fun v -> Hashtbl.replace verbs v.v_name v) !verb_tbl

let fini () =
	verb_tbl := [];
	Hashtbl.clear verbs
