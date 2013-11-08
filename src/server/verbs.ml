(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Utils
open Model
open Printf
open Direction
open Aperture

let emitl = Game.emitl

let level_change ~actor ~cur_level ~new_level =
	let rise = new_level > cur_level in
		Model.post_event (Level (actor, new_level, rise));
		emitl actor "Level has changed."
		(* FIXME : level msg *)

let level_check ~actor thunk =
	let cur_level = Props.get_level actor in
	let ignored = thunk () in
	let new_level = Props.get_level actor in
		ignore(ignored);
		if cur_level <> new_level
		then level_change ~actor ~cur_level ~new_level

let set_prompt ~prompt =
	"\xffPRN" ^ prompt

let update_prompt ~actor =
	let prompt = Props.get_prompt actor in
		emitl actor (set_prompt ~prompt)

let exits ~actor =
	let links = 
		List.map string_of_direction 
			(List.map Model.direction_in_exit
				 (LinkSet.elements (Link.get_all_links (Tree.parent actor)))
			)
	in
		emitl actor
			(match links with
				| [] -> "You don't see any exits!"
				| [one] -> sprintf "The only exit is %s." one
				| _ -> sprintf "Exits lead %s." (Utils.comma_sep links))

let look ~actor =
	let parent = Tree.parent actor in
	let name = Name.vague (Props.get_name parent) in
	let code = Loc_code.string_of_loc_code (Props.get_loc_code parent) in
	let title = name ^ " (" ^ code ^ ")" in
		if Light.player_can_see actor
		then (
			emitl actor title;
			emitl actor (Props.get_description parent);
			Tree.iter_children parent 
				(fun i -> 
					if i != actor
					then emitl actor (String.capitalize (Props.here_msg i)));
			List.iter (emitl actor) (Link.portalled_exit_descriptions parent);
			exits ~actor
		)	
		else (
			emitl actor "It is too dark to see anything."
		)

let go_dir dir ~actor =
	(* this doesn't work - the event fires even if the action is impossible *)
(*	let src = Tree.parent actor in*)
	try
(*		Model.post_event (Depart (actor, dir)); *)
(*		Events.dispatch_events (); (* cheat *) *)
		Link.move_dir actor dir;
		look ~actor;
(*		Model.post_event (Arrive (actor, src)) *)
	with
		| No_exit -> emitl actor "There is no exit that way!"
		| Portal_not_open mo -> 
			emitl actor (Grammar.render ~actor:mo "Ouch!  %Au %vpres:be not open.")

let take ~actor ~patient =
	Tree.take ~actor ~patient

let drop ~actor ~patient =
	Tree.drop ~actor ~patient

let drop_all ~actor =
	Tree.drop_all ~actor

(* temporary hack *)
let pdrop ~actor ~patient =
	let value = Props.get_value patient in
	let pit_room = World.get_a_room World.ThePit in
	let depo_room = World.get_a_room World.UnderThePit in
	let parent = Tree.parent actor in
		if parent == pit_room
		then (
			Model.post_event (Drop (actor, patient));
			Tree.insert_into ~recipient:depo_room patient;
			level_check ~actor (fun () -> 
									Props.stat_incr actor Persona.score value);
			Props.set_pitted patient
		)
		else emitl actor "You have to be at the pit to do that."

let probe ~actor ~patient =
	let value = Props.get_value patient in
	let fmt = Printf.sprintf "%%Au %%vpres:be worth %d." value in
		emitl actor (Grammar.render ~actor:patient fmt)

let quit ~actor =
	emitl actor "Bwye!";
	Model.quit ~actor

let examine ~actor ~patient =
  emitl actor (Props.get_description patient)

let close_ap ~actor ~patient =
	try
		let s = Props.change_item_aperture_state patient `Shut in
			emitl actor (Grammar.render ~patient s)
	with Model.No_aperture ->
		emitl actor (Grammar.render ~patient "You can't close %pu.")

let open_ap ~actor ~patient =
	try 
		let s = Props.change_item_aperture_state patient `Open in
			emitl actor (Grammar.render ~patient s)
	with Model.No_aperture ->
		emitl actor (Grammar.render ~patient "You can't open %pu.")

let scry_ap ~actor ~patient =
	Model.cast_spell ~actor ~spell_name:"scry";

	let msg = 
		try
			let ac = Props.get_aperture_accessor patient in
				"As you think, " ^ (Model.Props.get_vague_name ac) ^ " comes to mind."
		with 
			| No_aperture -> Grammar.render ~actor:patient "%Au %vpres:can not be opened"
			| No_accessor -> Grammar.render ~actor:patient "%Au %vpres:can not be locked"
	in
		emitl actor msg

let unknown_command ~actor =
  emitl actor "Sorry, I don't understand you."

let contents_list objects =
	comma_sep (List.map Model.Props.get_vague_name objects)

let inventory ~actor =
  let inv = Tree.children actor in
    match List.length inv with
      | 0 -> emitl actor "You aren't carrying anything."
      | _ -> emitl actor ("You are carrying " ^
								 (contents_list inv)
			  ^  ".") 
	  
let score ~actor =
	let sc = Props.stat_get actor Persona.score in
		emitl actor "Your score is:";
		emitl actor (string_of_int sc)
		
let cheat ~actor =
	level_check ~actor (fun () -> Props.stat_incr actor Persona.score 1000);
	Props.stat_incr actor Persona.gold 1000

(*
let tick ~actor =
	let count = ref 5 in
	let tick_fn = fun ~self -> 
					  count := !count - 1;
					  Model.post_event (Tick);
					  if !count <= 0
					  then Daemon.stop self
	in
	let dmn = Daemon.create ~frequency:3.0 ~tick_fn in
		Daemon.start dmn
*)

(*
let when_next ~actor =
	let tp = Workqueue.top_priority () in
	let report = Printf.sprintf "Next tick at: %10.10f" tp in
		emitl actor report
*)

let gamestat ~actor =
	(* version, boot time, last reset, war, cloak, players,
	   players since shutdown, pitted, hell, sun, moon *)
	let sun_state = Weather.sun () in
	let moon_state = Weather.moon () in
	let weather = Weather.global_weather in
		emitl actor (Weather.sun_msg sun_state);
		emitl actor (Weather.moon_msg moon_state);
		emitl actor (sprintf "\r\nWind: %d" !weather.Weather.w_wind);
		emitl actor (sprintf "Rain: %d" !weather.Weather.w_rain);
		emitl actor (sprintf "Clouds: %d" !weather.Weather.w_clouds);
		emitl actor (sprintf "Temperature: %d" !weather.Weather.w_temperature);
		emitl actor (sprintf "\r\nObjects: %d" (Props.count ()))

let teleport ~actor ~patient =
	Model.Tree.insert_into ~recipient:patient actor;
	emitl actor "You teleport!";
	let fmt = "%Au %vpres:teleport to %pu!" in
		emitl actor (Grammar.render ~actor ~patient fmt)

let unlock ~actor ~patient ~instrument =
	let aps = Model.Props.get_aperture_state patient in
		match aps with
			| Locked -> 
				  let key = Model.Props.get_aperture_accessor patient in
					  if key != instrument
					  then emitl actor "It doesn't work."
					  else (
						  let s = Model.Props.change_item_aperture_state patient `Unlock in
							  emitl actor (Grammar.render ~patient s)
					  )
			| _ -> emitl actor (Grammar.render ~actor:patient "%Au %vpres:be not locked.")

(* FIXME: traps *)
let lock ~actor ~patient ~instrument =
	let aps = Model.Props.get_aperture_state patient in
		match aps with
			| Locked -> emitl actor (Grammar.render ~actor:patient "%au %vpres:be already locked.")
			| Closed -> 
				  let key = Model.Props.get_aperture_accessor patient in
					  if key != instrument
					  then emitl actor "It doesn't work."
					  else (
						  let s = Model.Props.change_item_aperture_state patient `Lock in
						  emitl actor (Grammar.render ~patient s)
					  )
			| _ -> emitl actor (Grammar.render ~actor:patient "%Au %vpres:have to be closed before locking.")

let mstatus ~actor ~patient =
	emitl actor (Grammar.render ~patient "You regard %pu.");
	let count = List.length (Tree.children patient) in
		emitl actor (Printf.sprintf "Children: [%d]" count)
		
let superget ~actor ~patient =
	if Tree.free patient
	then (
		Tree.insert_into ~recipient:actor patient;
		emitl actor (Grammar.render ~actor ~patient "You superget %pu.")
	)
	else emitl actor "That object isn't free."

let drive ~actor ~patient =
	try Vehicle.drive actor patient;
		emitl actor (Grammar.render ~patient "You get into %pu.")
	with 
		| Already -> emitl actor "You must disembark your current vehicle!"
		| Item_not_vehicular -> emitl actor "You can't use that as a vehicle."
				  
let undrive ~actor =
	try let patient = Vehicle.undrive actor in
			emitl actor (Grammar.render ~patient "You get out of %pu.")
	with Already -> emitl actor "You're not driving anything anyway!"

(* FIXME: need output! *) (* FIXME - prompt should be in function, etc *)
let wizmode ~actor =
	Props.set_wizmode actor true;
	update_prompt ~actor

let unwizmode ~actor =
	Props.set_wizmode actor false;
	update_prompt ~actor

let delay ~actor =
	emitl actor (Delay.do_delay ~delay:5.0)

let whereis ~actor ~word =
	let free_tally tly =
		List.iter (fun (room, count) ->
			let msg = sprintf "%d in the location known as %s." 
				count (Props.get_vague_name room)
			in
				emitl actor msg
		) tly
	in

	let unfree_tally tly =
		emitl actor "(* FIXME *)"
	in

	let unfree_tally_non_wiz = function
		| 0 -> ()
		| n -> emitl actor (sprintf "%d hidden in other objects." n)
	in

	let wizmode = Props.is_in_wizmode actor in
		emitl actor ("You search for something sounding like `" ^ word ^ "'.");
		let results = 
			Search.search_all ~ty:(Some MO_Item) ~name:(Some word) @
			Search.search_all ~ty:(Some MO_Monster) ~name:(Some word)	
		in
		let free_ones, unfree_ones = List.partition Model.Tree.free results in
		let free_ones = List.map Tree.parent free_ones in
		let unfree_ones = List.map Tree.parent unfree_ones in
			free_tally (Utils.tally free_ones);
			match wizmode with
				| false -> unfree_tally_non_wiz (List.length unfree_ones)
				| true -> unfree_tally (Utils.tally unfree_ones)

let locate ~actor ~word =
	emitl actor ("Locations sounding like `" ^ word ^ "':");
	let describe_loc i =
		let loc_code = Model.Props.get_loc_code i in
		let msg = (Model.Props.get_unvague_name i) ^ " (" ^
			(Loc_code.string_of_loc_code loc_code) ^ ")"
		in
		emitl actor msg
	in
	let results = Search.search_all ~ty:(Some MO_Room) ~name:(Some word) in
		match results with
			| [] -> emitl actor "There doesn't seem to be any!"
			| _ -> List.iter describe_loc results

(* FIXME:
   ~word should be ~line or something
   need to fix punctuation where sentence doesn't end in fullstop
*)

let say ~actor ~word =
	let parent = Tree.parent actor in
	let utterance = Sentence.create word in
		Model.post_mudobject_event parent (Say (actor, utterance))

let emote ~actor ~word =
	let parent = Tree.parent actor in
		Model.post_mudobject_event parent (Emote (actor, word))

(* FIXME:
   should go through events
*)

let shout ~actor ~word =
	Model.post_event (Model.Shout (actor, word))

let tell ~actor ~patient ~word =
	let person = Grammar.Second in
	let fmt = "%Au %vpres:tell %pu \x22" ^ word ^ "\x22" in
		emitl actor (Grammar.render ~actor ~patient ~person fmt)

let start_quest ~actor =
	(* FIXME: already on quest, disbarred, etc *)
	let new_quest = Props.get_next_quest actor in
		Props.set_quest actor (Some new_quest);
		emitl actor "You start your quest.";
		update_prompt ~actor

let stop_quest ~actor =
	(* FIXME: already on quest? *)
	emitl actor "You abandon your quest.";
	Props.set_quest actor None;
	update_prompt ~actor

let finish_quest ~actor =
	(* FIXME: already on quest? *)
	emitl actor "You complete your quest.";
	Props.set_quest actor None;
	level_check ~actor (fun () ->
							Props.stat_incr actor Persona.quests_done 1 );
	update_prompt ~actor

let status_report ~observer ~actor =
	let get = Props.stat_get actor in
	let get_name n = (Persona.get_stat_name n) ^ ":" in
	let sc = get Persona.score in
	let qc = get Persona.quests_done in
	let line_of_stats s1 s2 s3 =
		sprintf "%s     %10d  %s %10d  %s      %10d\n"
			(get_name s1) (get s1) 
			(get_name s2) (get s2) 
			(get_name s3) (get s3)
	in
	let guild = Model.Props.get_guild actor in
	let level = Props.get_level actor in
		[
			"Your score is:";
			(string_of_int sc);
			"Quests completed:";
			(string_of_int qc);
			"Level:";
			(Persona.string_of_level level);
			line_of_stats Persona.strength Persona.max_strength Persona.gold;	
			line_of_stats Persona.magic_strength Persona.max_magic Persona.score;
			line_of_stats Persona.intelligence Persona.dexterity Persona.skill;
			line_of_stats Persona.size Persona.capacity Persona.fright;
			line_of_stats Persona.sin Persona.debt Persona.hoopiness;
			"Guild: " ^ (Guild.string_of_guild guild);
		]

let status ~actor =
	List.iter (fun line -> emitl actor line) (status_report ~observer:actor ~actor)

let attack ~actor ~patient =
	Fight.set_opponent actor patient;
	emitl actor (Grammar.render ~patient "You attack %pu.")

(* test verb *) (* FIXME: stop daemon! *)
let exterminate ~actor ~patient =
	Model.destroy patient;
	emitl actor "Monster destroyed."

let fight ~actor ~word =
	let move = Combat_state.move_of_string word in
		Fight.do_move actor ~move

let addlink ~actor ~patient ~word =
	let dir = Direction.match_direction word in
	let dir_name = Direction.whither dir in
		Link.add_link (Tree.parent actor) patient dir None None;
		emitl actor ("You create a new exit " ^ dir_name ^ ".")

let collapse ~actor ~word =
	let dir = Direction.match_direction word in
	let dir_name = Direction.string_of_direction dir in
		Link.remove_link (Tree.parent actor) dir;
		emitl actor ("You collapse the exit to " ^ dir_name ^ ".")
	
let switch_on ~actor ~patient =
	try Props.light_change patient Light_fsm.SwitchOn;
	with _ -> emitl actor "Fail"

let switch_off ~actor ~patient =
	try Props.light_change patient Light_fsm.SwitchOff;
		emitl actor "You light (* FIXME *)"
	with _ -> emitl actor "Fail"
		
let room_graph ~actor =
	let rooms () = Search.all_by_type MO_Room in
	let code room = Loc_code.string_of_loc_code (Props.get_loc_code room) in
	let print_room_link src dst = 
		code src ^ " -> " ^ code dst 
	in
	let s = String.concat ";\n" (List.flatten (
		List.map (fun room ->
			List.map (fun dst ->
				print_room_link room dst) (Model.Link.outbound_neighbours room)
		) (rooms ())
	))
	in
	let ofh = open_out "/tmp/links.dot" in
		output_string ofh ("digraph islinks {\n" ^ s ^ ";\n}\n");
		close_out ofh
		
let buy_spell ~actor ~word =
	Model.buy_spell ~actor ~spell_name:word

let do_death ~actor =
	Model.Props.set_dead actor

(* FIXME 
   need to handle the case where there was no trap

*)
   
let unset_trap ~actor ~patient =
	try let new_state = Model.Props.change_item_aperture_state patient `Deactivate;
		in
			ignore(new_state);
			emitl actor "You deactivate the trap"
	with _ ->
		emitl actor "Something went badly wrong"
	
   
let set_trap ~actor ~patient =
	try let new_state = Model.Props.change_item_aperture_state patient `Trap;
		in
			ignore(new_state);
			emitl actor (Grammar.render ~actor ~patient "You set a trap on %pu.")
	with _ ->
		emitl actor "Something went badly wrong"
	
let list_players ~actor =
	let players = Game.current_players () in
		List.iter (fun p -> emitl actor (Model.Props.get_unvague_name p)) players

let list_people ~actor =
	list_players ~actor

(* patient: *)
let extract ~actor ~patient ~word =
	assert (Model.Props.is_aperture_open patient); (* FIXME *)

	let ty = Some Model.MO_Item in
	let name = Some word in
	let results = Search.search patient ~ty ~name ~and_parent:false ~exclusive:false in
		match results with
			| [] -> 
				emitl actor "You can't find anything called that. (* FIXME *)"
			| hd :: tl ->
				let recipient = Model.Tree.parent patient in
					Model.Tree.insert_into ~recipient hd;
					emitl actor "You extract it."
                    (* FIXME *)

(* FIXME: size *)
let insert ~actor ~patient ~instrument =
	assert (Model.Props.is_aperture_open patient); (* FIXME *)

	Tree.insert_into ~recipient:patient instrument;
	emitl actor (Grammar.render ~actor ~patient "You stick it into it") (* FIXME *)

let search_in_object ~actor ~patient =
	assert (Model.Props.is_aperture_open patient); (* FIXME *)

	let children = Tree.children patient in
		match (List.length children) with
			| 0 -> emitl actor "The FIXME doesn't contain anything"
			| _ -> emitl actor ("The FIXME contain(s) " ^
				(contents_list children) ^ ".")

let join_guild ~actor ~word =
	let guild = Guild.guild_of_string word in
	let current_guild = Model.guild_of_player actor in
		assert (Guild.can_join current_guild);
		assert (Guild.joinable guild);

		Model.join_guild actor ~guild

let run_shutdown ~actor =
	emitl actor "You trigger an immediate shutdown.";
	Reset.set_shutdown ()
