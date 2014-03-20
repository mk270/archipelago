(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Light
open Light_fsm

exception Not_a_light

type food_type =
	| Edible
	| Potable

type edibility = {
	ed_food_type : food_type ;
	ed_poisoned : bool ;
}

let food_template = {
	ed_food_type = Edible;
	ed_poisoned = false;
}

type t = {
	op_light : light_fsm option ;
	op_edibility : edibility option ;
	op_cursed : bool ;
	op_inscription : string option ;
	op_antisummoner : bool ;
	op_antiforcer : bool ;
	op_qobj : bool ;
	op_unstealable : bool ;
	mutable op_pitted : bool ;
	op_size : int ;
	op_base_value : int ;
	op_weapon_value : int ;
	op_worn : bool ;
	op_bodypart : bool ;
	op_vehicularity : Vehicle.vehicle option;
}

let template = {
	op_light = None;
	op_edibility = None;
	op_cursed = false;
	op_inscription = None;
	op_antisummoner = false;
	op_antiforcer = false;
	op_qobj = false;
	op_unstealable = false;
	op_pitted = false;
	op_size = 100;
	op_base_value = 0;
	op_weapon_value = 0;
	op_worn = false;
	op_bodypart = false;
	op_vehicularity = None;
}

let warn s =
	Printf.printf "unrecognised object flag: %s\n" s

(* fiat lux *)
let create_light state =
	Some (Light_fsm.create state)

let create ~flags ~stats =
	let props =
		List.fold_left 
			(fun a -> function
				 | "edible" -> 
					   let edit = match a.op_edibility with
						   | None -> food_template
						   | Some food -> food
					   in
						   { a with op_edibility = Some 
								   { edit with ed_food_type = Edible } }
				 | "potable" -> 
					   let edit = match a.op_edibility with
						   | None -> food_template
						   | Some food -> food
					   in
						   { a with op_edibility = Some 
								   { edit with ed_food_type = Potable } }
				 | "cursed" -> { a with op_cursed = true }
				 | "glowing" -> { a with op_light = create_light Glowing }
				 | "worn" -> { a with op_worn = true }
				 | "qobj" -> { a with op_qobj = true }
				 | "lightable" -> { a with op_light = create_light Lightable }
				 | "legible" -> { a with op_inscription = Some "(* FIXME *)" }
				 | "bodypart" -> { a with op_bodypart = true }
				 | "unstealable" -> { a with op_unstealable = true }
				 | "anti-summoner" -> { a with op_antisummoner = true }
				 | "trapped" -> a (* this is dealt with elsewhere *)
				 | "robust" -> { a with op_light = create_light RobustlyLightable }
					   (* robust implies "lightable not lit", in the DB *)
				 | s -> warn s ; a		
		) template flags
	in
	let props =
		List.fold_left 
			(fun a -> function
				 | ("0", v) -> { a with op_size = int_of_string v }
				 | ("1", v) -> { a with op_base_value = int_of_string v }
				 | ("2", v) -> { a with op_weapon_value = int_of_string v }
				 | ("vehicle", veh) -> { a with op_vehicularity = 
						   Some (Vehicle.vehicle_of_string veh) }
				 | (s, _) -> warn s ; a
		) props stats
	in
		props

let value props =
	match props.op_pitted with
		| false -> props.op_base_value
		| true -> 0

let set_pitted props =
	props.op_pitted <- true

let is_lit props =
	match props.op_light with
		| None -> false
		| Some l -> Light.is_on l.Utils.Fsm.fsm_state

let light_change props msg =
	match props.op_light with
		| None -> raise Not_a_light
		| Some l -> Utils.Fsm.send l msg

let vehicularity props =
	props.op_vehicularity
