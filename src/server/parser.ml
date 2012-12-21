(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Socket
open Model

(* this needs to become a tuple *)

type search_space =
	| Global
	| Siblings
	| Children
	| ParentLinks

type match_style =
	| ByName
	| ByCode

type arg' =
	| MO of match_style * mo_type list * search_space list
	| ArgLiteral

type arg =
  | ItemCarried    (* MO ([MO_Item;], [Children;]) *)
  | ItemInLocation (* MO ([MO_Item;], [Siblings;]) *)
  | ItemPresent    (* MO ([MO_Item;], [Children; Siblings;]) *)
  | ItemOrPortalPresent (* MO ([MO_Portal; MO_Item;], [ParentLinks; Children; Siblings;]) *)
  | LocationCode (* *)
  | MonsterPresent (* MO ([MO_Monster;], [Children; Siblings;]) *)
  | ItemAnywhere (* MO ([MO_Item;], [Global;]) *)
  | Literal (* ArgLiteral *)
  | CurrentPlayer
  | PlayerPresent

let search_pattern = function
  | ItemCarried    -> MO (ByName, [MO_Item;], [Children;])
  | ItemInLocation -> MO (ByName, [MO_Item;], [Siblings;])
  | ItemPresent    -> MO (ByName, [MO_Item;], [Children; Siblings;])
  | ItemOrPortalPresent -> 
		MO (ByName, [MO_Portal; MO_Item;], [ParentLinks; Children; Siblings;])
  | LocationCode -> MO (ByCode, [MO_Room;], [Global])
  | MonsterPresent -> MO (ByName, [MO_Monster;], [Children; Siblings;])
  | ItemAnywhere -> MO (ByName, [MO_Item;], [Global;])
  | CurrentPlayer -> MO (ByName, [MO_Player;], [Global;])
  | Literal -> ArgLiteral
  | PlayerPresent -> MO (ByName, [MO_Player;], [Siblings;])

type role =
  | Patient
  | Recipient
  | Instrument
  | Searchterm

type unary   = actor : mudobject -> unit

type binary  = actor : mudobject -> patient : mudobject -> unit

type binary_word = actor : mudobject -> word : string -> unit

type ditrans = actor : mudobject -> patient : mudobject -> 
    instrument : mudobject -> unit

type ditrans_word = actor : mudobject -> patient : mudobject -> word : string -> unit

type word =
	| Var of role * arg
	| Constant of string
	| Rest_of_line of role * arg

type frame = 
  | Unary   of unary
  | Binary  of binary
  | Ditrans of ditrans
  | Binary_word of binary_word
  | Ditrans_word of ditrans_word

type resolved_arg =
	| Mudobject of mudobject
	| Word of string

type verb = { 
	v_name : string ;
	v_args : word list ;
	v_frame : frame ;
}

let delim_ws   = Str.regexp " +"

let room_by_code name =
	try let code = Loc_code.create name in
		[ Search.room_by_code code; ]
	with Loc_code.Invalid_loccode _ -> raise Not_found

let search_method name = function
	| ByName -> Model.Props.match_name (Some name)
	| ByCode -> let loc_code = Loc_code.create (String.uppercase name) in
		  Model.Props.match_loc_code (Some loc_code)

let objs_in_search_space ~actor = function
	| Global -> Search.search_all ~ty:None ~name:None
	| Siblings -> 
		  List.filter (fun i -> i != actor) (Tree.children (Tree.parent actor))
	| Children ->
		  Tree.children actor
	| ParentLinks -> 
		  Model.MudobjectSet.elements (Link.portals_in_room (Tree.parent actor))

let resolve_arg ~actor arg name =
	let sp = search_pattern arg in
		match sp with
			| ArgLiteral -> Word name
			| MO (meth, sought_types, search_spaces) ->
				  let f = search_method name meth in
				  let objs = fun i -> objs_in_search_space ~actor i in
				  let objects = List.map objs search_spaces in
				  let objects = List.flatten objects in
					  (* we now have all potentially searchable objects *)
				  let objects = List.filter f objects in
				  let filtered = List.map (
					  fun ty -> List.filter (fun o -> Model.mudobj_ty_match o (Some ty)) objects
				  ) sought_types in
				  let filtered = List.flatten filtered in
					  if List.length filtered = 0
					  then raise Not_found
					  else Mudobject (List.hd filtered)

let mudobject_of_resolved_arg = function
	| Mudobject mo -> mo
	| _ -> failwith "Mudobject expected"

let word_of_resolved_arg = function
	| Word s -> s
	| _ -> failwith "Word expected"

exception Too_many_words
exception Constant_not_found

let exec verb ~actor line =
	let rxp = Str.regexp " +" in
	let do_match token s =
		match token with
			| Var (th, arg) -> 
				  let words' = Str.bounded_split rxp s 2 in
				  let obj = resolve_arg ~actor arg (List.hd words') in
					  (match words' with
						  | [] -> failwith "no words left to parse"
						  | [hd] -> (Some (th, obj), "")
						  | hd :: tl -> 
								(Some (th, obj), List.hd tl))
			| Constant s' ->
				  let words' = Str.bounded_split rxp s 2 in
					  (match words' with
						   | [] -> failwith "no worlds left to parse"
						   | [hd] -> 
								 (if Utils.initial_match hd s'
								  then (None, "")
								  else raise Constant_not_found)
						   | hd :: tl ->
								 (if Utils.initial_match hd s'
								  then (None, List.hd tl)
								  else raise Constant_not_found)
					  )
			| Rest_of_line (th, arg) ->
				  if 1 > String.length s
				  then failwith "no words left to parse"
				  else (Some (th, Word s), "")
	in				  
	let rec parse s tokens acc =
		match tokens with
			| [] -> acc (* ignore case of unparsed words at end *)
			| hd :: tl ->
				  let role, rest_of_line = do_match hd s in
					  match role with
						  | Some r -> parse rest_of_line tl (r :: acc)
						  | None   -> parse rest_of_line tl (acc)
	in
	let get_pairs () =
		let tmp = Str.bounded_split rxp line 2 in
		let line' = match tmp with
			| [] -> failwith "no words on line"
			| [hd] -> failwith "only one word on line"
			| hd :: tl ->
				  assert (List.length tl = 1);
				  List.hd tl
		in
			parse line' verb.v_args [] 
	in
	let m_o_r_a = mudobject_of_resolved_arg in
	let w_o_r_a = word_of_resolved_arg in
		match verb.v_frame with
			| Unary cmd -> cmd ~actor
			| Binary cmd ->
				  let pairs = get_pairs () in
				  let patient = m_o_r_a (List.assoc Patient pairs) in
					  cmd ~actor ~patient
			| Ditrans cmd ->
				  let pairs = get_pairs () in
				  let patient = m_o_r_a (List.assoc Patient pairs) in
				  let instrument = m_o_r_a (List.assoc Instrument pairs) in
					  cmd ~actor ~patient ~instrument
			| Binary_word cmd ->
				  let pairs = get_pairs () in
				  let word = w_o_r_a (List.assoc Patient pairs) in
					  cmd ~actor ~word
			| Ditrans_word cmd ->
				  let pairs = get_pairs () in
				  let patient = m_o_r_a (List.assoc Patient pairs) in
				  let word  = w_o_r_a (List.assoc Instrument pairs) in
					  cmd ~actor ~patient ~word

let guard_exceptions ~actor e =
	let msg = "An error occurred: " ^ (Printexc.to_string e) in
	let bt = Printexc.get_backtrace () in
		emitl actor msg;
		print_endline bt;
		print_endline msg;
		flush_all ()

let guard_exceptions ~actor = function
	| Persona.Spell_failed (Persona.Spell_not_available) ->
		Socket.emitl actor "You don't know that spell."
	| Persona.Spell_failed (Persona.Restricted r) -> 
		Socket.emitl actor "Restricted"
	| e -> guard_exceptions ~actor e
