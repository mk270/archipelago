(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Model
open Grammar

let get_person ~observer ~actor =
	if observer == actor
	then Second
	else Third

let new_moon ~observer moon_state =
	() (* currently we don't report this to the player *)

let new_weather ~observer weather_state =
	Weffect.update weather_state observer

let pickup ~observer ~actor ~patient =
	let s = 
		if observer == actor
		then render ~actor ~patient "You %vpres:pick up %pu."
		else render ~actor ~patient "%Au %vperf:pick up %pu."
	in
		Socket.emitl observer s
(*	let fmt = "%Au %vpres:pick up %pu." in
		Communication.wall2 ~actor ~patient ~local:true fmt  *)

let drop ~observer ~actor ~patient =
	let person = get_person ~observer ~actor in
	let s = match person with
		| Second -> "%Au %vpres:drop %pu."
		| Third -> "%Au %vperf:drop %pv."
		| _ -> failwith "Invalid person"
	in
		Socket.emitl observer (render ~actor ~patient ~person s)

let depart ~observer ~actor ~dir =
	if observer != actor
	then
		let w = Direction.whither dir in
		let s = Grammar.render ~actor "%Au %vperf:go " ^ w ^ "." in
			Socket.emitl observer s

let disturb room =
	(* send a ping to any monster present *)
	let monsters = Search.search room ~ty:(Some MO_Monster)
		~name:None ~and_parent:false ~exclusive:false
	in
	let f mo = not (Props.is_active mo) in
	let monsters = List.filter f monsters in
	let wake recipient = 
		(* enqueue ~recipient ~event:Wakeup *)
		Model.post_mudobject_event recipient Model.Wakeup
	in
		List.iter wake monsters

let arrive ~observer ~actor ~src =
	if observer != actor
	then
		let dst = Model.Tree.parent actor in
		let dir = Model.Link.dir_from_source ~src ~dst in
		let w = match dir with
			| Some dir -> Direction.whence dir
			| None -> "somewhere"
		in
		let s = Grammar.render ~actor "%Au %vperf:arrive from " ^ w ^ "." in
		let r = Tree.parent actor in
		let neighbours = Link.neighbours r in
			Socket.emitl observer s;
			if Model.mudobj_ty_match actor (Some MO_Player)
			then List.iter disturb (r :: neighbours)

let do_say ~observer ~actor ~line =
	let quoted = match line with
		| Sentence.Exclamation s -> "%Au %vpres:exclaim \"" ^ s ^ "\""
		| Sentence.Query s -> "%Au %vpres:ask \"" ^ s ^ "\""
		| Sentence.Statement s -> "%Au %vpres:say \"" ^ s ^ "\""
	in
	let person = get_person ~observer ~actor in
	let quoted = Grammar.render ~actor ~person quoted in
		Socket.emitl observer quoted

let do_emote ~observer ~actor ~line =
	let quoted = Grammar.render ~actor ("%Au " ^ line) in
	let s = 
		if observer == actor
		then "You emote: " ^ quoted
		else quoted
	in
		Socket.emitl observer s

let do_shout ~observer ~actor ~line =
	let person = get_person ~observer ~actor in
	let quoted = Grammar.render ~actor ~person ("%Au %vpres:shout ") in
	let quoted = quoted ^ "\"" ^ line ^ "\"" in
		Socket.emitl observer quoted

let level_change ~observer ~actor ~level ~rise =
	let v = match rise with
		| true -> "rise"
		| false -> "fall"
	in
	let lev = Persona.string_of_level level in
	let fmt = "%Au %vperf:" ^ v ^ " to level " ^ lev ^ "." in
		if observer == actor
		then Socket.emitl actor (render ~actor fmt)
		else Socket.emitl actor (render ~actor fmt)

let combat_result ~observer ~result =
	let s = Combat_state.string_of_response result in
		Socket.emitl observer ("Damage report: " ^ s)

let logon ~observer ~actor =
	if observer != actor
	then Socket.emitl observer (render ~actor "%Av %vperf:log on.")	

let logout ~observer ~actor =
	if observer != actor
	then Socket.emitl observer (render ~actor "%Av %vperf:log out.")

let do_death ~observer ~actor ~cause =
	if observer != actor
	then Socket.emitl observer (render ~actor "%Av %vperf:be killed.")

(* this message is probably redundant *)
let new_sun ~observer sun_state =
	let sun_msg = Weather.new_sun_msg sun_state in
	let sun_msg = sun_msg ^ "." in (* should also check terrain, for "outside"*)
		Socket.emitl observer sun_msg

let wakeup ~observer =
	if Model.mudobj_ty_match observer (Some Model.MO_Monster)
	then Monster.add_daemon observer

let dispatch_event_to_observer ~observer = function
	| Logon actor -> logon ~observer ~actor
	| Logout actor -> logout ~observer ~actor
	| Arrive (actor, src) -> arrive ~observer ~actor ~src
	| Depart (actor, dir) -> depart ~observer ~actor ~dir
	| Pickup (actor, patient) -> pickup ~observer ~actor ~patient
	| Drop (actor, patient) -> drop ~observer ~actor ~patient
	| Wakeup -> wakeup ~observer
	| CombatResult result -> combat_result ~observer ~result
	| NewSun s -> new_sun ~observer s
	| NewMoonState s -> new_moon ~observer s
	| NewWeather s -> new_weather ~observer s
	| Say (actor, line) -> do_say ~observer ~actor ~line
	| Emote (actor, line) -> do_emote ~observer ~actor ~line
	| Shout (actor, line) -> do_shout ~observer ~actor ~line
	| Level (actor, level, rise) -> level_change ~observer ~actor ~level ~rise
	| Death (actor, cause) -> do_death ~observer ~actor ~cause

let dispatch_mudobject_event ~recipient ev =
	let observers = 
		if Model.mudobj_ty_match recipient (Some Model.MO_Room)
		then (Model.Tree.children recipient)
		else [recipient]
	in
		List.iter (fun observer -> 
			dispatch_event_to_observer ~observer ev) observers

let dispatch_events () =
	let events = Model.unbuffer_events () in
	let f ev =
		List.iter (fun recipient -> 
			dispatch_mudobject_event ~recipient ev) (Socket.current_players ())
	in
		Queue.iter f events

let dispatch_mudobject_events () =
	let dirty = Model.unbuffer_dirty_mudobjects () in
		List.iter (fun recipient ->
			let buf = Model.unbuffer_mudobject_events recipient in
				Queue.iter (fun ev -> dispatch_mudobject_event ~recipient ev) buf
		) dirty

let pump_all () =
	let rec try_to_quiesce = function
		| 0 -> print_endline "Taking too many rounds to quiesce!"
		| n -> 
			if Model.quiescent ()
			then ()
			else (dispatch_events ();
				  dispatch_mudobject_events ();
				  try_to_quiesce (n - 1))
	in
		try_to_quiesce 4
