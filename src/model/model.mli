(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Direction
open Name
open Aperture

type combat_info

type
	mudobject
and
	link
and
	aperture
and
	fight
and
	cause_of_death =
	| Killed_by of mudobject
	| Died_of of string
and
	exits
and
	event =
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

type edge

type mo_type = MO_Room | MO_Item | MO_Player | MO_Monster | MO_Portal | MO_Link

module LinkSet      : Set.S with type elt = edge
module MudobjectSet : Set.S with type elt = mudobject

exception No_exit
exception No_aperture
exception Portal_not_open of mudobject
exception Not_a_room
exception Not_a_player
exception No_accessor
exception Item_required_for_passage
exception Already
exception Item_not_vehicular

module Create : sig
	val create_room : name -> desc : string -> loc_code : string -> terrain : string -> mudobject
	val create_item : name -> desc : string -> flags : string list -> 
		stats : (string * string) list -> mudobject
	val create_player : name -> mudobject
	val create_monster : name -> sex : Sex.sex -> mudobject
	val create_portal : name -> aperture_state -> mudobject
	val create_aperture : aperture_state -> mudobject option -> aperture

end

module Search : sig
	val search : mudobject -> ty:mo_type option -> name:string option
		-> and_parent : bool -> exclusive:bool -> mudobject list

	val room_by_code : Loc_code.t -> mudobject

	val search_all :
		ty : mo_type option ->
		name : string option ->
		mudobject list
			
	val by_id : int -> mudobject
	val all_by_type : mo_type -> mudobject list
end

module Link : sig
	val add_link : mudobject -> mudobject -> direction -> 
		mudobject option -> mudobject option -> unit
	val remove_link : mudobject -> direction -> unit
	val get_all_links : mudobject -> LinkSet.t
	val portals_in_room : mudobject -> MudobjectSet.t
	val portal_in_link : link -> mudobject option
	val portalled_exit_descriptions : mudobject -> string list
	val move_dir : mudobject -> direction -> unit
	val apstate_of_string : string -> aperture_state
	val reachable : src : mudobject -> dst : mudobject -> bool
	val connected : src : mudobject -> dst : mudobject -> bool
	val neighbours : mudobject -> mudobject list
	val outbound_neighbours : mudobject -> mudobject list
	val dir_to_destination : src : mudobject -> dst : mudobject -> direction
	val dir_from_source : src : mudobject -> dst : mudobject -> direction option
	val portal_in_direction : src : mudobject -> dir : direction -> mudobject
end

module Tree : sig
	val children : mudobject -> mudobject list
	val parent : mudobject -> mudobject
	val insert_into : recipient : mudobject -> mudobject -> unit
	val remove_from : parent : mudobject -> mudobject -> unit
	val map_children : mudobject -> (mudobject -> 'a) -> 'a list
	val iter_children : mudobject -> (mudobject -> unit) -> unit
	val free : mudobject -> bool
	val take : actor : mudobject -> patient : mudobject -> unit
	val drop : actor : mudobject -> patient : mudobject -> unit
	val drop_all : actor : mudobject -> unit
end

module Vehicle : sig
	val get_player_vehicle : mudobject -> mudobject option
	val set_player_vehicle : mudobject -> mudobject option -> unit
	val get_item_vehicularity : mudobject -> Vehicle.vehicle option
	val drive : mudobject -> mudobject -> unit
	val undrive : mudobject -> mudobject
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
	val get_aperture_accessor : mudobject -> mudobject
	val get_aperture_state : mudobject -> aperture_state
	val is_aperture_open : mudobject -> bool

	val stat_incr : mudobject -> Persona.stat -> int -> unit
	val stat_get : mudobject -> Persona.stat -> int
	val match_name : string option -> mudobject -> bool
	val match_loc_code : Loc_code.t option -> mudobject -> bool

	val set_pitted : mudobject -> unit
	val get_value : mudobject -> int

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
end

val destroy : mudobject -> unit
val destroy_all : unit -> unit
val mudobj_ty_match : mudobject -> mo_type option -> bool
val quit : actor : mudobject -> unit

module Light : sig
	val player_can_see : mudobject -> bool
end

module Fight : sig
	val set_opponent : mudobject -> mudobject -> unit
	val do_move : mudobject -> move : Combat_state.combatant_move -> unit
	val is_fighting : mudobject -> bool
	val set_next_deadline : combat_info -> unit
	val handle_msg : msg : Combat_state.combat_message -> mudobject -> unit
	val get_fight : mudobject -> fight
	val get_combat_info : fight -> combat_info
	val is_due : now : float -> combat_info -> bool
end

val post_event : event -> unit
val unbuffer_events : unit -> event Queue.t
val unbuffer_mudobject_events : mudobject -> event Queue.t
val post_mudobject_event : mudobject -> event -> unit
val unbuffer_dirty_mudobjects : unit -> mudobject list
val quiescent : unit -> bool

val cast_spell : actor : mudobject -> spell_name : string -> unit
val buy_spell : actor : mudobject -> spell_name : string -> unit

val direction_in_exit : edge -> direction

val join_guild : mudobject -> guild : Guild.t -> unit
val guild_of_player : mudobject -> Guild.t
val exits_of_mudobject : mudobject -> exits
