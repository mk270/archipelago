(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Model

let log s =
	print_endline s;
	flush_all ()

(*
  this needs to reworked such that:
    it has no weird infinite tail recursion bug due to currying
    it uses a general FSM
    it can pick up on events other than timer
    it stores a bit of state per mobile daemon
*)
(*
move, combat, parser, conversation, body parts
*)

(*
  when players move around, we shall tap rooms they could move to

  if a monster is in a tapped room, it shall check if there are unbound
    players nearby, and bind to a free one

  if a bound monster is not in the same room as its bindee, nor can reach
    the same "easily", it shall unbind

  if a bound monster isn't in the same room as its bindee, but can reach it
    is should move there

  present bound monsters:
    shall periodically do their atmospherics

    shall consider whether to attack the bindee / flee

    shall notice when their bindee leaves, and decide if to follow

    shall notice what their bindee says, and respond if necessary

*)

type monster_state = {
	ms_controlled : Model.mudobject;
	mutable ms_heartbeats : int;
	mutable ms_next_run_time : float;
}

module MonsterControllerSet = Set.Make (
	struct
		type t = monster_state
		let compare x y = 
			let x = x.ms_controlled in
			let y = y.ms_controlled in
				compare (Props.get_id x) (Props.get_id y)
	end
)

let frequency = 3.0
let monster_daemons = ref MonsterControllerSet.empty

let report_monster_daemons () =
	let report_daemon state =
		let monster = state.ms_controlled in
			Printf.printf "mudobj: %s (%d)\nTTL: %d ; wakeup: %f\n\n"
				(Props.get_vague_name monster)
				(Props.get_id monster)
				state.ms_heartbeats state.ms_next_run_time
	in
		log "---begin daemon report---";
		MonsterControllerSet.iter report_daemon !monster_daemons;
		log "---end daemon report---";
		flush_all ()

let get_next_run_time () =
	Unix.gettimeofday () +. frequency

let create mo = 
	let next_time = get_next_run_time () in
	let controller = {
		ms_controlled = mo;
		ms_heartbeats = 5;
		ms_next_run_time = next_time;
	} in
		if MonsterControllerSet.mem controller !monster_daemons
		then
			(log ("attempted duplicate daemon on " ^ (Model.Props.get_vague_name mo));
		     controller)
		else
			(monster_daemons := MonsterControllerSet.add controller !monster_daemons;
			 controller)		

let lose_heart ~state =
	state.ms_heartbeats <- state.ms_heartbeats - 1

let get_bindee ~actor =
	Model.Props.bound_to actor

let perform ~actor ~bindee =
	log "doing perform";
	Model.post_event (Model.Say (actor, Sentence.create "Hwi!"));
	() (* atmospheric, attack/free, respond *)

let move_to ~actor ~my_parent ~dst =
	let dir = Link.dir_to_destination ~src:my_parent ~dst in
		try Link.move_dir actor dir
		with Model.No_exit ->
			Printf.printf "Couldn't exit: %s [%s] -> [%s]\n" (Direction.string_of_direction dir) (Props.get_vague_name my_parent) (Props.get_vague_name dst)
			

let pursue_bindee ~actor ~bindee ~state ~my_parent ~his_parent =
	log "doing pursue";
	lose_heart ~state;
	let dests = Link.neighbours my_parent in
	let dests = List.filter (fun i -> i != my_parent) dests in
		if List.memq his_parent dests
		then move_to ~actor ~my_parent ~dst:his_parent
		else match dests with
			| [] -> ()
			| _ -> let dst = Utils.random_list_member dests in
					   move_to ~actor ~my_parent ~dst

let interact_with_bindee ~actor ~state =
	let bindee = get_bindee ~actor in
		let his_parent = Model.Tree.parent bindee in
		let my_parent = Model.Tree.parent actor in
			if his_parent != my_parent
			then pursue_bindee ~actor ~bindee ~state ~my_parent ~his_parent
			else perform ~actor ~bindee

let bind ~actor ~patient =
	log (Printf.sprintf "bind %s -> %s" (Props.get_vague_name actor) (Props.get_vague_name patient));
	Model.Props.bind_to actor patient

let player_bound ~player =
	Model.Props.is_bound player

let search room =
	Model.Search.search room ~ty:(Some Model.MO_Player)
	~name:None ~and_parent:false ~exclusive:false

let players_local ~actor = 
	search (Model.Tree.parent actor)

let players_nearby ~actor =
	let par = Model.Tree.parent actor in
	let rooms = par :: Model.Link.neighbours par in
		List.flatten (List.map search rooms)

let unbound_players_nearby ~actor =
	let players_nearby ~actor = [] in
	let players = (players_local ~actor) @ (players_nearby ~actor) in
		List.filter (fun i -> not (player_bound ~player:i)) players

let seek_bindable_player ~actor ~state =
	let players_available = unbound_players_nearby ~actor in
		match players_available with
			| [] -> lose_heart ~state
			| hd :: tl -> bind ~actor ~patient:hd

let monster_bound ~actor =
	Model.Props.is_bound actor

let stop_daemon ~actor ~state =
	Model.Props.set_active actor false;
	monster_daemons := MonsterControllerSet.remove state !monster_daemons;
	log "monster daemon stopping"
	
let monster_tick ~actor ~state =
	(try (match monster_bound ~actor with
		| false -> seek_bindable_player ~actor ~state
		| true -> interact_with_bindee ~actor ~state)
	with e ->
		log ("Monster error: " ^ (Printexc.to_string e)));
	if state.ms_heartbeats <= 0
	then stop_daemon ~actor ~state
	else state.ms_next_run_time <- get_next_run_time ()

let monster_daemon_init ~monster =
	let state = create monster in
		monster_tick ~actor:monster ~state

let add_daemon monster =
	let ms = Model.Props.get_vague_name monster in
		if not (Model.Props.is_active monster)
		then (log ("consider adding daemon on " ^ ms);
			  monster_daemon_init ~monster)
              (* else ignore *)

let check_for_timeouts () =
	let now = Unix.gettimeofday () in
	let f = fun state -> 
		if now > state.ms_next_run_time
		then let actor = state.ms_controlled in
				 monster_tick ~actor ~state		
	in
		MonsterControllerSet.iter f !monster_daemons

let set_timeout () =
	let f elt a =
		min a elt.ms_next_run_time
	in
	let first_timeout = MonsterControllerSet.fold f !monster_daemons 0.0 in
		if first_timeout > 0.0
			(* FIXME: implement post_absolute *)
		then let delay = first_timeout -. Unix.gettimeofday () in
				 Game.workqueue_post ~delay (fun () -> ())	

(* this should enumerate the monster daemons and kick them *)
let pump_events () =
		report_monster_daemons ();
		check_for_timeouts ();
		set_timeout ()
