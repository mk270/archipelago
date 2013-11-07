(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Node
open Direction
open Name
open Weather
open Aperture

(* ultimately, the submodules in here might be best as separate files

   the material in this file is kept together largely to allow the
   submodules to call each other without exposing all their interfaces
   globally, which, now that I've stated it, sounds less like a bad idea 
*)

type graph_label =
	| Exit
	| Contained_in
	| Unlocked_by
	| Scenery
	| Has_portal
	| Requires_obj

let graph_type = function
	| Exit -> Graph
	| Contained_in -> Tree
	| Unlocked_by -> Tree
	| Scenery -> Graph
	| Has_portal -> Tree
	| Requires_obj -> Tree

type mo_type = MO_Room | MO_Item | MO_Player | MO_Monster | MO_Portal | MO_Link

type local_weather = {
	lw_weather : Weather.weather;
	lw_time_of_day : Weather.time_of_day;
}


type entity =
	| Room
	| Item
	| Player
	| Monster
	| Portal
	| Link

type loc_info =
		{ 
			loci_loc_code : Loc_code.t;
			loci_terrain : Terrain.terrain;
			loci_vehicle_required : Vehicle.vehicle option;
		}

type combat_info =
		{
			cmbi_state : Combat_state.combat_fsm;
			mutable cmbi_next_round_due : float;
		}

type player =
		{
			player_persona : Persona.persona;
			player_state : Player_state.t;
			mutable player_logging_out : bool;
			mutable player_local_weather : local_weather;
		}

type mudobject =
		{
			mo_entity : entity;
			mutable mo_neighbours : (mudobject, graph_label) node option;
			mo_name : name;
			mo_id : int;
			mo_sex : Sex.sex ;
			mo_description : string option;
			mo_monster_actions : bool option;
			mo_item_properties : Item_prop.t option;
			mutable mo_aperture : aperture option;
			mo_player : player option;
			mo_loc_info : loc_info option;
			mo_body : bool option; (* TBD *)
			mutable mo_fight : fight option;
			mutable mo_bound_to : mudobject option;
			mutable mo_monster_active : bool;
			mutable mo_vehicle : mudobject option;
			mo_link_direction : direction option;
		}
and
	aperture = 
		{ 
			ap_fsm : aperture_fsm;
			ap_key : mudobject option;
		}
and
	fight =
		{
			ft_opponent : mudobject;
			ft_combat_info : combat_info;
			ft_combat_role : Combat_state.combatant_id;
		}
(*
type body =
		{
			bd_heads : int;
			bd_eyes : int; (* CDBD eyes? *)
			bd_arms : int;
			bd_hands : int;
			bd_legs : int;
			bd_feet : int;
			bd_behind : bool; (* unbelievably, ISLAND went into considerable
							   gynaecological detail on these points *)
			bd_breasts : bool;
			bd_male_organ : bool;
			bd_female_organ : bool;
			bd_wings : bool;
			bd_tail : bool;
(*			mutable bd_others : mudobject list;
			mutable bd_clothing : mudobject list; *)
		}
*)

type cause_of_death =
	| Killed_by of mudobject
	| Died_of of string

type event =
	| Logon of mudobject
	| Logout of mudobject
	| Pickup of mudobject * mudobject
	| Drop of mudobject * mudobject
	| Depart of mudobject * direction
	| Arrive of mudobject * mudobject
	| NewSun of Weather.time_of_day
	| NewMoonState of Weather.phase_of_moon
	| NewWeather of Weather.weather
	| Say of mudobject * Sentence.sentence
	| Emote of mudobject * string
	| Shout of mudobject * string
	| Level of mudobject * Persona.level * bool
	| Wakeup
	| CombatResult of Combat_state.combat_round_result
	| Death of mudobject * cause_of_death

exception No_exit
exception No_aperture
exception Portal_not_open of mudobject
exception Not_a_room
exception Not_a_player
exception No_accessor
exception No_item_properties
exception Vehicle_required
exception Item_not_vehicular
exception Already
(* ## exception Not_allowed_on_quest *)
exception Not_a_wizard
exception Item_required_for_passage
exception Not_fighting
exception Already_bound
exception Not_bound
exception Not_free
exception Not_present
exception Link_missing_direction

let universe = ref []
let global_msg_queue = Queue.create ()
let dirty = ref []

let msg_queues : (int, event Queue.t) Hashtbl.t = Hashtbl.create 5


(* pretty much every use of this function ought to be replaced by
   something more exact *)
let assert_entity_type ty mo = 
	let entity = mo.mo_entity in
		match ty, entity with
			| MO_Room, Room -> ()
			| MO_Item, Item -> ()
			| MO_Player, Player -> ()
			| MO_Monster, Monster -> ()
			| MO_Portal, Portal -> ()
			| MO_Link, Link -> ()
			| _ -> assert false

let exits_of_mudobject mo = 
	match mo.mo_neighbours with
	| Some n -> List.map Node.contained (Node.destinations_of n Exit)
	| None -> assert false

let player_of_mudobject mo =
	match mo.mo_player with
		| Some p -> p
		| _ -> raise Not_a_player

let aperture_from_mudobject i =
	match i.mo_aperture with
		| Some a -> a
		| None -> raise No_aperture

let node_of_mudobject mo =
	match mo.mo_neighbours with
		| Some c -> c
		| None -> failwith "Uninitialised object: no node!"

let mudobj_ty_match mo ty =
	match ty, mo.mo_entity with
		| None, _ -> true
		| Some MO_Room, Room -> true
		| Some MO_Item, Item -> true
		| Some MO_Player, Player -> true
		| Some MO_Monster, Monster -> true
		| Some MO_Portal, Portal -> true
		| _, _ -> false

let string_of_entity mo =
	match mo.mo_entity with
		| Player -> "player"
		| Item -> "object"
		| Monster -> "monster"
		| Room -> "location"
		| Portal -> "door"
		| Link -> "link"

let string_of_apstate = function
	| Open -> "open"
	| Closed -> "closed"
	| Trapped -> "trapped"
	| Locked -> "locked"

let aperture_open = function
	| Open -> true
	| _ -> false

(* module dependencies:
   Create
   Props
   Tree
   Search :- Props, Tree
   Vehicle :- Tree
   Link :- Props, Tree, Vehicle
   Light :- Props, Tree, Search
   Fight
*)

let post_event ev =
	Queue.add ev global_msg_queue

let unbuffer_events () =
	let buf = Queue.create () in
		Queue.transfer global_msg_queue buf;
		buf

let unbuffer_dirty_mudobjects () = 
	let too_dirty_to_clean_my_act_up = !dirty in
		dirty := [];
		let rec unique acc = function
			| [] -> acc
			| hd :: tl ->
				if List.memq hd acc
				then unique acc tl 
				else unique (hd :: acc) tl
		in
			unique [] too_dirty_to_clean_my_act_up

let quiescent () =
	(0 = List.length !dirty) && (Queue.is_empty global_msg_queue)

(* let direction_in_exit ex = ex.exit_direction *)

module Create : sig
	val create_room : name -> desc : string -> loc_code : string -> terrain : string -> mudobject
	val create_item : name -> desc : string -> flags : string list -> 
		stats : (string * string) list -> mudobject
	val create_player : name -> mudobject
	val create_monster : name -> sex : Sex.sex -> mudobject
	val create_portal : name -> aperture_state -> mudobject
	val create_aperture : aperture_state -> mudobject option -> aperture
	val create_link : (mudobject, graph_label) node -> direction -> 
		(mudobject, graph_label) node option -> 
		(mudobject, graph_label) node option -> mudobject

end = 
struct
	let obj_id = ref 0

	let next_id () =
		obj_id := !obj_id + 1;
		!obj_id

	let entity_template = {
		mo_entity = Room;
		mo_neighbours = None;
		mo_name = ("Missing name!", NoAdam, Singular);
		mo_id = -1;
		mo_sex = Sex.Neuter;
		mo_description = None;
		mo_monster_actions = None;
		mo_item_properties = None;
		mo_aperture = None;
		mo_player = None;
		mo_loc_info = None;
		mo_body = None;
		mo_fight = None;
		mo_bound_to = None;
		mo_monster_active = false;
		mo_vehicle = None;
		mo_link_direction = None;
	}

	let wrap_entity ~mo =
		let id = next_id () in
		let mo = { mo with mo_id = id; } in
		let c = create graph_type mo in
			mo.mo_neighbours <- Some c;
			universe := mo :: !universe;
			mo

	let create_room name ~desc ~loc_code ~terrain =
		let v = String.sub loc_code 1 1 in
		let loci = {
			loci_loc_code = Loc_code.create loc_code;
			loci_terrain = Terrain.create terrain;
			loci_vehicle_required = 
				(try Some (Vehicle.vehicle_of_string v)
				 with Vehicle.Invalid_vehicle -> None);
		} in
		let mo = { entity_template with
					   mo_description = Some desc;
					   mo_entity = Room;
					   mo_name = name;
					   mo_loc_info = Some loci;
				 } in
			wrap_entity ~mo
				
	let create_item name ~desc ~flags ~stats =
		let mo = { entity_template with
					   mo_description = Some desc;
					   mo_entity = Item;
					   mo_name = name;
					   mo_item_properties = Some (Item_prop.create ~flags ~stats);
					   mo_aperture = None;
				 } in
			wrap_entity ~mo
				
	let create_player name =
		let mo = { entity_template with
					   mo_entity = Player;
					   mo_player = Some { 
						   player_persona = Persona.create ();
						   player_state = Player_state.create ();
						   player_logging_out = false;
						   player_local_weather = {
							   lw_weather = Weather.default_weather;
							   lw_time_of_day = Weather.sun ();
						   };
					   };
					   mo_name = name;
				 } in
			wrap_entity ~mo
				
	let create_monster name ~sex =
		let mo = { entity_template with
					   mo_entity = Monster;
					   mo_name = name;
					   mo_monster_actions = Some false;
					   mo_sex = sex;
				 } in
			wrap_entity ~mo

	let create_aperture ap_state accessor =
		let ap_fsm = Fsm.create ap_state ap_inp_handler in
			{ ap_fsm = ap_fsm;
			  ap_key = accessor; }
				
	let create_portal name ap_state =
		let mo = { entity_template with
					   mo_entity = Portal;
					   mo_name = name;
					   mo_aperture = Some (create_aperture ap_state None);
				 } in
			wrap_entity ~mo

	let create_link dst dir portal obj_required =
		let mo = { entity_template with
			mo_entity = Link;
			mo_name = ("link", Name.Indefinite, Name.Singular);
			mo_link_direction = Some dir;
		} in
		let ent = wrap_entity ~mo in
		let src = node_of_mudobject ent in
			Node.insert_into dst src Exit;
			(match portal with
				| None -> ()
				| Some p -> Node.insert_into p src Has_portal);
			(match obj_required with
				| None -> ()
				| Some o -> Node.insert_into o src Requires_obj);
			ent

end


module Props : sig
	val get_id : mudobject -> int
	val get_name : mudobject -> name
	val get_vague_name : mudobject -> string
	val get_unvague_name : mudobject -> string
	val get_description : mudobject -> string

	val here_msg : mudobject -> string

	val set_logout : mudobject -> unit
	val get_logout : mudobject -> bool

	val set_aperture : mudobject -> aperture option -> unit
	val change_item_aperture_state : mudobject -> ap_message -> string
	val get_aperture_state : mudobject -> aperture_state
	val get_aperture_accessor : mudobject -> mudobject
	val is_aperture_open : mudobject -> bool

	val stat_incr : mudobject -> Persona.stat -> int -> unit
	val stat_get : mudobject -> Persona.stat -> int
	val match_name : string option -> mudobject -> bool
	val match_loc_code : Loc_code.t option -> mudobject -> bool

	val set_pitted : mudobject -> unit
	val get_value : mudobject -> int

	val is_lit : mudobject -> bool

	val get_loc_code : mudobject -> Loc_code.t

	val get_local_weather : mudobject -> Weather.weather
	val set_local_weather : mudobject -> Weather.weather -> unit

	val get_time_of_day : mudobject -> Weather.time_of_day
	val set_time_of_day : mudobject -> Weather.time_of_day -> unit

	val get_terrain : mudobject -> Terrain.terrain

	val count : unit -> int
	val set_wizmode : mudobject -> bool -> unit
	val is_in_wizmode : mudobject -> bool

	val get_prompt : mudobject -> string
	val get_next_quest : mudobject -> string
	val set_quest : mudobject -> string option -> unit

	val get_level : mudobject -> Persona.level

	val bind_to : mudobject -> mudobject -> unit
	val bound_to : mudobject -> mudobject
	val is_bound : mudobject -> bool
	val unbind : mudobject -> unit

	val is_active : mudobject -> bool
	val set_active : mudobject -> bool -> unit
	val light_change : mudobject -> Light_fsm.light_msg -> unit

	val set_dead : mudobject -> unit
	val is_dead : mudobject -> bool

	val get_guild : mudobject -> Guild.t

end = struct

	let get_id mo = mo.mo_id
	let get_name mo = mo.mo_name
	let get_vague_name mo = Name.vague mo.mo_name
	let get_unvague_name mo = Name.unvague mo.mo_name

	let get_description mo =
		match mo.mo_description with
			| None -> "No description available (* FIXME *)"
			| Some desc -> desc

	let stat_incr mo stat d =
		let pl = player_of_mudobject mo in
			Persona.stat_incr pl.player_persona stat d

	let stat_get mo stat =
		let pl = player_of_mudobject mo in
			Persona.stat_get pl.player_persona stat

	let here_msg mo = here_msg mo.mo_name

	let set_logout mo =
		let p = player_of_mudobject mo in
			p.player_logging_out <- true

	let get_logout mo =
		let p = player_of_mudobject mo in
			p.player_logging_out

	let set_aperture mo ap =
		if (mudobj_ty_match mo (Some MO_Item) || 
			mudobj_ty_match mo (Some MO_Portal))
		then mo.mo_aperture <- ap
		else (
			let err = "Attempt to use aperture on " ^ (string_of_entity mo) in
				failwith err
		)

	let change_item_aperture_state mo state_change =
		let a = aperture_from_mudobject mo in
		let aps = a.ap_fsm in
			Fsm.send aps state_change

	let get_aperture_state mo =
		let a = aperture_from_mudobject mo in
		let ap_fsm = a.ap_fsm in
			ap_fsm.Fsm.fsm_state

	let get_aperture_accessor mo =
		let a = aperture_from_mudobject mo in
			match a.ap_key with
				| None -> raise No_accessor
				| Some o -> o

	let is_aperture_open mo =
		match (get_aperture_state mo) with
			| Open -> true
			| _ -> false

	let match_name n mo =
		match n with 
			| None -> true
			| Some nom -> Utils.initial_match nom (obj_name mo.mo_name)
(*			| Some nom -> obj_name mo.mo_name = nom *)

	let match_loc_code lc mo =
		match lc with
			| None -> true
			| Some loc_code ->
				  match mo.mo_loc_info with
					  | None -> false
					  | Some loci -> loci.loci_loc_code = loc_code

	let set_pitted mo =
		match mo.mo_item_properties with
			| None -> raise No_item_properties
			| Some props -> Item_prop.set_pitted props

	let get_value mo =
		match mo.mo_item_properties with
			| None -> raise No_item_properties
			| Some props -> Item_prop.value props

	let is_lit mo =
		match mo.mo_item_properties with
			| None -> raise No_item_properties
			| Some p -> Item_prop.is_lit p

	let get_loc_code mo =
		match mo.mo_loc_info with
			| None -> raise Not_a_room
			| Some loci -> loci.loci_loc_code

	let get_local_weather mo =
		let player = player_of_mudobject mo in
			player.player_local_weather.lw_weather

	let get_time_of_day mo =
		let player = player_of_mudobject mo in
			player.player_local_weather.lw_time_of_day

	let set_local_weather mo w =
		let player = player_of_mudobject mo in
		let tod = get_time_of_day mo in
			player.player_local_weather <- {
				lw_weather = w;
				lw_time_of_day = tod;
			}

	let set_time_of_day mo tod =
		let player = player_of_mudobject mo in
		let weather = get_local_weather mo in
			player.player_local_weather <- {
				lw_weather = weather;
				lw_time_of_day = tod;
			}

	let get_terrain mo =
		match mo.mo_loc_info with
			| None -> raise Not_a_room
			| Some loci -> loci.loci_terrain	

	let count () =
		List.length !universe

	let set_wizmode mo state =
		let pl = player_of_mudobject mo in
		let ps = pl.player_state in
		let wiz = Persona.is_wizard pl.player_persona in (* FIXME *)
			if not wiz
			then raise Not_a_wizard
			else Player_state.set_wizmode ps state

	let is_in_wizmode mo =
		let pl = player_of_mudobject mo in
		let ps = pl.player_state in
			Player_state.in_wizmode ps

	let get_prompt mo =
		let pl = player_of_mudobject mo in
			Player_state.get_prompt pl.player_state

	let get_next_quest mo =
		ignore(player_of_mudobject mo);
		"A quest"

	let set_quest mo quest =
		let pl = player_of_mudobject mo in
			match quest with
				| Some q -> Player_state.set_quest pl.player_state q
				| None -> Player_state.unset_quest pl.player_state

	let get_level mo =
		let pl = player_of_mudobject mo in
			Persona.get_level pl.player_persona

	let bind_to mo victim =
		match mo.mo_bound_to with
			| Some _ -> raise Already_bound
			| None -> 
				match victim.mo_bound_to with
					| None -> (mo.mo_bound_to <- Some victim;
							   victim.mo_bound_to <- Some mo)
					| Some _ -> raise Already_bound

	(* on read, we should check the two-way link and update if broken *)
	let bound_to mo =
		match mo.mo_bound_to with
			| None -> raise Not_bound
			| Some p -> p

	let is_bound mo =
		match mo.mo_bound_to with
			| None -> false
			| Some _ -> true

	let unbind mo =
		match mo.mo_bound_to with
			| None -> raise Not_bound
			| Some other ->
				match other.mo_bound_to with
					| None -> () (* FIXME: should log an error *)
					| Some orig ->
						if orig == mo
						then (
							mo.mo_bound_to <- None;
							other.mo_bound_to <- None;
						) (* FIXME: else log error *)

	let is_active mo =
		mo.mo_monster_active

			(* needs FSM checks *)
	let set_active mo yn =
		mo.mo_monster_active <- yn

	let light_change mo msg =
		match mo.mo_item_properties with
			| None -> raise No_item_properties
			| Some props -> Item_prop.light_change props msg
(*
	let change_level mo ~old_level =
		let new_level = get_level mo in
		let rising = new_level > old_level in
			post_event (NewLevel (mo, new_level, rising))

	let with_level_check mo f =
		let old_level = get_level mo in
			f ();
			let new_level = get_level mo in
				if old_level != new_level
				then change_level mo ~old_level
*)

	let set_dead mo =
		let pl = player_of_mudobject mo in
			Player_state.set_dead pl.player_state true

	let is_dead mo =
		let pl = player_of_mudobject mo in
			Player_state.is_dead pl.player_state

	let get_guild mo =
		let pl = player_of_mudobject mo in
			Persona.get_guild pl.player_persona

end


let post_mudobject_event mo ev =
	let id = Props.get_id mo in
	let q = 
		(if not (Hashtbl.mem msg_queues id)
		then Hashtbl.replace msg_queues id (Queue.create ()));
		Hashtbl.find msg_queues id
	in
		Queue.add ev q;
		dirty := mo :: !dirty

let unbuffer_mudobject_events mo =
	let id = Props.get_id mo in
	let buf = Queue.create () in
	let q = 
		try Hashtbl.find msg_queues id
		with Not_found -> (
			print_endline "disaster";
			Queue.create ()			)
	in
		Queue.transfer q buf;
		buf

type edge = { exit_direction : direction ;
			  exit_link : mudobject }

module LinkSet = Set.Make (
	struct
		type t = edge
		let compare x y = 
			compare 
				(int_of_direction (x.exit_direction)) 
				(int_of_direction (y.exit_direction))
	end
)

module MudobjectSet = Set.Make (
	struct
		type t = mudobject
		let compare x y = compare x.mo_id y.mo_id
	end
)

module Tree : sig
	val children : mudobject -> mudobject list
	val parent : mudobject -> mudobject
	val insert_into : recipient : mudobject -> mudobject -> unit
	val remove_from : parent : mudobject -> mudobject -> unit
	val map_children : mudobject -> (mudobject -> 'a) -> 'a list
	val iter_children : mudobject -> (mudobject -> unit) -> unit
	val free : mudobject -> bool (* FIXME: rename to is_free *)
	val take : actor : mudobject -> patient : mudobject -> unit
	val drop : actor : mudobject -> patient : mudobject -> unit
	val drop_all : actor : mudobject -> unit

end = struct

	let map_children mo f =
		let c = node_of_mudobject mo in
			List.map (fun i -> f (contained i)) (Node.sources_of c Contained_in)

	let iter_children mo f =
		ignore(map_children mo (fun i -> ignore(f i)))

	let children mo =
		map_children mo (fun x -> x)

	let parent mo =
		let c = node_of_mudobject mo in
			match (Node.destinations_of c Contained_in) with
				| [] -> raise Not_found
				| [p] -> contained p
				| _ -> assert false
(*
	let stop_using_object parent child =
		try let pl = player_of_mudobject parent in
			print_endline "checking for dropped vehicle";
			match pl.player_vehicle with
				| None -> ()
				| Some v -> 
					  if v == child
					  then pl.player_vehicle <- None
		with Not_a_player -> ()
*)
	let insert_into ~recipient child =
		let p = node_of_mudobject recipient in
		let c = node_of_mudobject child in
			Node.insert_into p c Contained_in
				
	let remove_from ~parent child =
		let p = node_of_mudobject parent in
		let c = node_of_mudobject child in
			Node.remove_from p c Contained_in

	let free mo =
		match mo.mo_entity with
			| Room -> false
			| _ -> 
				  try let parent' = parent mo in
					  match parent'.mo_entity with
						  | Room -> true
						  | _ -> false
				  with Not_found -> true

	let take ~actor ~patient =
		let room = parent actor in
			if not (free patient)
			then raise Not_free
			else if (parent patient) != room
			then raise Not_present
			else (
				post_mudobject_event room (Pickup (actor, patient));
				insert_into ~recipient:actor patient
			)

	let drop ~actor ~patient =
		let room = parent actor in
			if not (parent patient == actor)
			then raise Not_present
			else (
				post_mudobject_event room (Drop (actor, patient));
				insert_into ~recipient:(parent actor) patient
			)

	let drop_all ~actor =
		List.iter (fun patient -> drop ~actor ~patient) (children actor)

end

module Search : sig
	val search : 
		mudobject ->
		ty : mo_type option ->
		name : string option ->
		and_parent : bool ->
		exclusive : bool ->
		mudobject list

	val room_by_code : Loc_code.t -> mudobject

	val search_all :
		ty : mo_type option ->
		name : string option ->
		mudobject list

	val by_id : int -> mudobject

	val all_by_type : mo_type -> mudobject list

end =
struct
    
	(* presumably there is a more elegant approach than this 
	   
	   f = (fun i -> (f i) & (new_function i))

	   stuff below? *)

	(* FIXME *)

	let search mo f =
		List.filter f (Tree.children mo)

	let search mo f ~and_parent =
		match and_parent with
			| false -> search mo f
			| true  -> (search mo f) @ (search (Tree.parent mo) f)

	let search mo f ~name ~and_parent =
		let f = (fun i -> (f i) & (Props.match_name name i))
		in
			search mo f ~and_parent
				
	let search mo f ~ty ~name ~and_parent =
		let f = (fun i -> (f i) & (mudobj_ty_match i ty))
		in
			search mo f ~name ~and_parent
				
	let search mo ~ty ~name ~and_parent ~exclusive =
		let f =
			match exclusive with
				| false -> (fun i -> true) 
				| true -> (fun i -> i != mo)
		in
			search mo f ~ty ~name ~and_parent


	let room_by_code code =
		let results = List.filter (
			fun mo -> match mo.mo_loc_info with
				| None -> false
				| Some loci -> loci.loci_loc_code = code
		) !universe
		in
			List.hd results (* obviously this should actually be a hashlookup *)

	let search_all ~ty ~name =
		List.filter 
			(fun mo -> match ty, name, mo with
				 | None, nom, mo -> Props.match_name nom mo
				 | Some t, nom, mo -> 
					   mudobj_ty_match mo ty && Props.match_name nom mo
			) !universe

	let by_id id =
		let rec find = function
			| [] -> raise Not_found
			| hd :: tl ->
				if id = Props.get_id hd
				then hd
				else find tl
		in
			find !universe	 

	let all_by_type ty =
		let rec find acc = function
			| [] -> acc
			| hd :: tl ->
				if mudobj_ty_match hd (Some ty)
				then hd :: find acc tl
				else find acc tl
		in
			find [] !universe
end

module Vehicle : sig
	val get_player_vehicle : mudobject -> mudobject option
	val set_player_vehicle : mudobject -> mudobject option -> unit
	val get_required_vehicle : mudobject -> Vehicle.vehicle option
	val get_item_vehicularity : mudobject -> Vehicle.vehicle option
	val drive : mudobject -> mudobject -> unit
	val undrive : mudobject -> mudobject
end = 
struct

	let set_player_vehicle mo vh =
			mo.mo_vehicle <- vh

				(* FIXME: needs to work for mobs too *)
	let get_player_vehicle mo =
		match mo.mo_vehicle with
			| None -> None
			| Some vh ->
				if mo == Tree.parent vh
				then Some vh
				else (mo.mo_vehicle <- None;
					  None)

	let get_required_vehicle mo =
		match mo.mo_loc_info with
			| None -> raise Not_a_room
			| Some loci -> loci.loci_vehicle_required

	let get_item_vehicularity mo =
		match mo.mo_item_properties with
			| None -> raise No_item_properties
			| Some props -> Item_prop.vehicularity props

	let drive actor patient =
		let curvh = get_player_vehicle actor in
			match curvh with
				| Some vh -> raise Already
				| None ->
					let vhity = get_item_vehicularity patient in
						match vhity with
							| None -> raise Item_not_vehicular
							| Some vh -> set_player_vehicle actor (Some patient)

	let undrive actor =
		let curvh = get_player_vehicle actor in
			match curvh with
				| None -> raise Already
				| Some patient -> 
					set_player_vehicle actor None;
					patient

end

module Link =
struct

	let direction_of_exit mo =
		match mo.mo_link_direction with
		| Some dir -> dir
		| None -> raise Link_missing_direction

	let edge_of_exit mo =
		let dir = direction_of_exit mo in
			(dir, mo)

	let add_link (src : mudobject) (dst :mudobject) (dir : direction) door req_item =
		let src_exits = exits_of_mudobject src in
		let dst_exits = exits_of_mudobject dst in
		let src_dirs = List.map direction_of_exit src_exits in
		let dst_mo = node_of_mudobject dst in
			ignore(dst_exits); (* force room *)
			assert (not (List.mem dir src_dirs));
		let door_mo =
			(match door with 
				 | Some d -> 
					 assert_entity_type MO_Portal d;
					 Some (node_of_mudobject d)
				 | None -> None) in
		let req_item_mo =
			(match req_item with
				 | Some d -> 
					 assert_entity_type MO_Item d;
					 Some (node_of_mudobject d)
				 | None -> None) in
			ignore (Create.create_link dst_mo dir door_mo req_item_mo)

	let remove_link mo dir =
		let src = node_of_mudobject mo in
		let src_exits = exits_of_mudobject mo in
		let src_dirs = List.map direction_of_exit src_exits in
		let right_dir ex = (dir = direction_of_exit ex) in
			match (List.filter right_dir src_exits) with
				| [] -> failwith "exit not found"
				| [hd] -> 
					let dst = node_of_mudobject hd in
						Node.remove_from dst src Exit
				| _ -> failwith "two exits in same direction?"
	(*			assert (List.mem dir src_dirs);*)

(*	
	let dest_in_link l =
		l.lnk_destination
*)

	let portal_in_link mo =
		let n = node_of_mudobject mo in
			match (Node.destinations_of n Has_portal) with
				| [] -> None
				| [hd] -> Some (Node.contained hd)
				| _ -> assert false

	let get_all_links mo =
		let exits = List.map edge_of_exit (exits_of_mudobject mo) in
			List.fold_left (
				fun a (x, y) -> LinkSet.add { 
					exit_direction = x; 
					exit_link = y 
				} a
			) LinkSet.empty exits
				
	let get_links ~with_portals mo =
		fst (LinkSet.partition (
				 fun elt -> let portal = portal_in_link (elt.exit_link) in
					 match portal, with_portals with
						 | Some _, true -> true
						 | None, false -> true
						 | _ -> false
			 ) (get_all_links mo))
			
	let portals_in_room mo =
		let links = get_links ~with_portals:true mo in
		
			LinkSet.fold (
				fun elt a -> let portal = portal_in_link (elt.exit_link) in
					match portal with
						| Some p ->	MudobjectSet.add p a
						| None -> a
			) links MudobjectSet.empty
			
	let description_of_exit dir lnk =
		let whither = whither dir in
		let ptl = portal_in_link lnk in
			match ptl with
				| None -> "A non-descript exit leads " ^ whither ^ "."
					  (* FIXME: plural *)
				| Some p -> 
(*					  let ap = match p.mo_aperture with
						  | None -> assert false
						  | Some a -> a
					  in*)
					  (String.capitalize (Props.get_vague_name p)) ^ ", which is " ^ 
					  (string_of_apstate (Props.get_aperture_state p)) ^ ", leads " ^ whither ^ "."
							  
	let exit_descriptions exits =
		LinkSet.fold (fun edge a -> (description_of_exit edge.exit_direction edge.exit_link) :: a) exits []
			
	let portalled_exit_descriptions mo =
		let exits_w_portals = get_links ~with_portals:true mo in
		let portals = portals_in_room mo in
			
		let link_having_portal ptl =
			let matching_links, _ = LinkSet.partition ( 
				fun edge -> let ptl' = portal_in_link edge.exit_link in
					match ptl' with
						| None -> assert false
						| Some p -> (Props.get_id ptl) = (Props.get_id p)
			) exits_w_portals in
				LinkSet.min_elt matching_links
		in
		let f ptl a = 
			LinkSet.add (link_having_portal ptl) a
		in
		let portalled_exits =
			MudobjectSet.fold f portals LinkSet.empty
		in
			exit_descriptions portalled_exits

	let portal_passable = function
		| None -> true
		| Some door -> 
			  try aperture_open (Props.get_aperture_state door)
			  with No_aperture -> false

	(* this must only be called when you're sure it'll work *)
	let portal_of_some = function
		| None -> assert false
		| Some p -> p

	let vehicular_travel_permitted ~pl_vehicle ~dst_vehicularity ~cur_vehicularity =
		match dst_vehicularity, cur_vehicularity, pl_vehicle with
			| None, Some cv, Some pv -> cv = pv
			| dv, _, pv -> dv = pv

	(* FIXME: Event to rooms being entered and left *)
	let move_dir mo dir =
		let exits = exits_of_mudobject (Tree.parent mo) in
		let l = (try Hashtbl.find exits dir
				 with Not_found -> raise No_exit)
		in
		let dst_vehicularity = Vehicle.get_required_vehicle l.lnk_destination in
		let current = Tree.parent mo in
		let cur_vehicularity = Vehicle.get_required_vehicle current in
		let pl_vehicle = 
			match Vehicle.get_player_vehicle mo with
				| None -> None
				| Some vh -> Vehicle.get_item_vehicularity vh
		in
			(match l.lnk_required_item with
				 | Some o -> 
					   if mo != Tree.parent o
					   then raise Item_required_for_passage
				 | None -> ());
			if vehicular_travel_permitted ~pl_vehicle ~dst_vehicularity ~cur_vehicularity
			then 
				(if portal_passable l.lnk_portal
				 then 
						(
							let src = Tree.parent mo in
							let dst = l.lnk_destination in
								Printf.printf "src: %d; dst %d\n" (Props.get_id src) (Props.get_id dst); flush_all ();
								post_mudobject_event src (Depart (mo, dir));
								Tree.insert_into ~recipient:dst mo;
								post_mudobject_event dst (Arrive (mo, src))
						)
				 else 
					 let ptl = portal_of_some l.lnk_portal in
						 raise (Portal_not_open ptl))
			else raise Vehicle_required

	(* FIXME: presumably this should be somewhere else *)
	let apstate_of_string = function
		| "locked" -> Locked
		| "shut" -> Closed
		| "open" -> Open
		| _ -> assert false

	let reachable ~src ~dst =
		let exits = exits_of_mudobject src in
			Hashtbl.fold (fun _ v a -> (dst == dest_in_link v) || a) exits false

	let connected ~src ~dst =
		reachable ~src ~dst && reachable ~dst ~src

	let neighbours' f mo =
		let exits = exits_of_mudobject mo in
		let exits' = Hashtbl.fold (
			fun _ v a -> let dst = dest_in_link v in
							 if f ~src:mo ~dst
							 then MudobjectSet.add dst a
							 else a
		) exits MudobjectSet.empty in
			MudobjectSet.elements exits'

	let neighbours mo = 
		neighbours' (fun ~src ~dst -> connected ~src ~dst) mo

	let outbound_neighbours mo = 
		neighbours' (fun ~src ~dst -> reachable ~src ~dst) mo

	let dir_to_destination ~src ~dst =
		let directions = Hashtbl.fold (
			fun dir dest a ->
				if dst == dest_in_link dest
				then dir :: a
				else a
		) (exits_of_mudobject src) [] in
			List.hd directions (* will raise Not_found appropriately *)

	let dir_from_source ~src ~dst =
		let directions = Hashtbl.fold (
			fun dir dest a ->
				if src == dest_in_link dest
				then dir :: a
				else a			
		) (exits_of_mudobject dst) [] in
			try Some (List.hd directions)
			with Failure _ -> None
					
	let portal_in_direction ~src ~dir =
		let exits = exits_of_mudobject src in
		let link = 
			try Hashtbl.find exits dir
			with Not_found -> failwith "No portal in that direction"
		in
			match portal_in_link link with
				| Some mo -> mo
				| None -> failwith "No portal in that direction"

end

let destroy mo =
	let id = Props.get_id mo in
		Hashtbl.remove msg_queues id;
		Tree.remove_from ~parent:(Tree.parent mo) mo; (* FIXME *)
		universe := Utils.remove mo !universe

let destroy_all () =
	let destroy i =
		try Tree.remove_from ~parent:(Tree.parent i) i
		with _ -> ()
	in
		List.iter destroy !universe;
		universe := []

(* crossreferences between mudobjects:
   exits, aperture keys, monster vulnerabilities
   fight opponent
   binder/bindee
   portals
   vehicle in use (weapon, shield...)
   body parts / clothes
   events
   event queue
   session
   monster daemons
   current players list
*)

(* ##
let references ~referer ~referent =
	true
*)

(* whether you can see:

   if there is light in the location
      OR
      you can see in the dark for some reason
      OR
      there is an object present which is emitting light
      OR
      - a seer is present (!) (we might want to do without this)
*)

(* there is light in a location if:
   - it's naturally lit by virtue of terrain
   OR
   - it's daytime and the terrain is not in perma darkness
   OR
   - it's nighttime and the moon isn't obscured by clouds
*)

(* you can see in the dark if
   - you're in wizmode
   - you're a necro
   - you're using the night spell
*)

(* there is a light source in a room if
    any player in the room is holding any object which is lit
    there is such an object standing free in the room
*)

(* type light_source =
   | Too_dark
   | Sunlight
   | Moonlight
   | Fullmoon_behind_clouds
   | Freestanding of mudobject
   | Held of mudobject * mudobject
*)

module Light : sig
	val player_can_see : mudobject -> bool
end =
struct
	let room_is_lit mo = 
		let time_of_day = Weather.sun () in
		let phase = Weather.moon () in
		let terrain = Props.get_terrain mo in
		let has_weather = Terrain.has_weather terrain in
			(* FIXME: should reference player's local weather *)
		let clouds = !Weather.global_weather.w_clouds in
		let cloudy = clouds >= 5 in
			match time_of_day, phase, has_weather, cloudy with
				| _, _, false, _ -> Terrain.is_permalit terrain
				| Night, Full, _, _ -> true
				| Night, NewMoon, _, _ -> false
				| Night, _, _, true -> false
				| Night, _, _, false -> not (Terrain.has_trees terrain)
				| _, _, true, _ -> true
					  
	let can_see_in_dark mo = 
		let pl = player_of_mudobject mo in
			Player_state.in_wizmode pl.player_state ||
				Player_state.in_nightmode pl.player_state ||
				Persona.is_necro pl.player_persona

	let light_in_room mo =
		let f = fun i -> Props.is_lit i in
		let any_true = List.fold_left (fun a b -> a || b) false in

		let objects_in mo =
			Search.search mo ~ty:(Some MO_Item) ~name:None ~and_parent:false ~exclusive:false
		in

		let players_in mo =
			Search.search mo ~ty:(Some MO_Player) ~name:None ~and_parent:false ~exclusive:false
		in
			
		let flatten l =
			let rec flatten l acc =
				match l with
					| [] -> acc
					| hd :: tl -> flatten tl (hd @ acc)
			in
				flatten l []
		in

		let held_objects = List.map objects_in (players_in mo) in
		let all_relevant_objects =
			(objects_in mo @ flatten held_objects)
		in
			any_true (List.map f all_relevant_objects)

	let player_can_see mo =
		let parent' = Tree.parent mo in
			room_is_lit parent' || can_see_in_dark mo || light_in_room parent'
end

module Fight : sig
	val set_opponent : mudobject -> mudobject -> unit
    (* FIXME probably don't need to expose this *)
	val get_fight : mudobject -> fight
	val do_move : mudobject -> move : Combat_state.combatant_move -> unit
	val handle_msg : msg : Combat_state.combat_message -> mudobject -> unit
	val is_fighting : mudobject -> bool
	val set_next_deadline : combat_info -> unit
	val get_combat_info : fight -> combat_info
	val is_due : now : float -> combat_info -> bool
end = struct

	let fight_timeout = 10.0

	let get_next_deadline () = fight_timeout +. Unix.gettimeofday ()

	let set_next_deadline cmbi =
		cmbi.cmbi_next_round_due <- get_next_deadline ()

	let set_opponent mo victim =
		assert_entity_type MO_Player mo;
		assert_entity_type MO_Player victim;
		(* FIXME: either already fighting; wrong types, etc *)

		let state = Combat_state.create_combatant_pair () in
		let combi = Fsm.create state Combat_state.combat_input_handler in 

		let setup me him role =	
			let combichrist = {
				cmbi_state = combi;
				cmbi_next_round_due = get_next_deadline ();
			} in
			let fight = {
				ft_opponent = him;
				ft_combat_info = combichrist;
				ft_combat_role = role;
			} in
				me.mo_fight <- Some fight
		in
			setup mo victim Combat_state.Initiator;
			setup victim mo Combat_state.Responder

				(* FIXME: need much better than this to deal
				   with the opponent getting lost *)
	let get_fight mo =
		match mo.mo_fight with
			| None -> raise Not_fighting
			| Some f -> f

	let handle_msg ~msg mo =
		let ft = get_fight mo in
		let role = ft.ft_combat_role in
		let ci = ft.ft_combat_info in
		let results = Fsm.send ci.cmbi_state msg in
		let my_response, his_response =
			match role with
				| Combat_state.Initiator ->
					fst results, snd results
				| Combat_state.Responder ->
					snd results, fst results
		in
			post_mudobject_event mo (CombatResult my_response);
			post_mudobject_event ft.ft_opponent (CombatResult his_response);
			set_next_deadline ci
	
	(* FIXME: check move valid *)
	let do_move mo ~move =
		let ft = get_fight mo in
		let role = ft.ft_combat_role in
		let msg = Combat_state.Move (move, role) in
			handle_msg ~msg mo

	let is_fighting mo =
		match mo.mo_fight with
			| Some _ -> true
			| None -> false

	let get_combat_info f =
		f.ft_combat_info

	let is_due ~now ci =
		ci.cmbi_next_round_due < now
end

(* FIXME: these want to be in modules *)

let lose_armour ~actor = ()
let lose_fight ~actor = ()
let drop_everything ~actor =
	Tree.drop_all ~actor

let kill_daemons ~actor = ()
let unbind ~actor = ()
(* ## let reward_killer ~actor ~killer = ()
let lose_life ~actor = () *)

let quit ~actor =
	lose_fight ~actor;
	lose_armour ~actor;
	(try ignore(Vehicle.undrive actor)
	with 
		| Already -> print_endline "error undriving: Already";
		| _ -> print_endline "error undriving");
	drop_everything ~actor;
	kill_daemons ~actor;
	unbind ~actor;
	Props.set_logout actor

(* ## ?
let death ~actor ~killer =
	reward_killer ~actor ~killer;
	lose_life ~actor;
	quit ~actor
*)

let cast_spell ~actor ~spell_name =
	let pl = player_of_mudobject actor in
	let persona = pl.player_persona in
	let wizmode = Player_state.in_wizmode pl.player_state in
		Persona.use_spell ~persona ~wizmode ~spell_name

let buy_spell ~actor ~spell_name =
	let pl = player_of_mudobject actor in
	let persona = pl.player_persona in
		Persona.buy_spell ~persona ~spell_name
(* ##
let string_of_cause_of_death = function
	| Killed_by k -> Props.get_vague_name k
	| Died_of s -> s
*)

let join_guild mo ~guild =
	let pl = player_of_mudobject mo in
		Persona.set_guild pl.player_persona ~guild

let guild_of_player mo =
	let pl = player_of_mudobject mo in
		Persona.get_guild pl.player_persona
