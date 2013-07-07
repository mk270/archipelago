(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


(* get list of fights;
     if any had its next_round_due before the current time:
       do the timeout action on it
       update last move time thing
*)


(* there's no good total ordering on combat state, so we use physical
   equality to distinguish them *)
let rec unique cmp = function 
      | [] -> []
      | x :: xs -> [x] @ (unique cmp (List.filter (fun y -> cmp x y) xs));;

(* FIXME: we should exclude broken fights here *)
let handle_timed_out_fight_rounds () =
	let now = Unix.gettimeofday () in
		
	let timeout ~player combat =
		Model.Fight.set_next_deadline combat;
		Model.Fight.handle_msg ~msg:Combat_state.Timeout player
	in
	let players = Session.current_players () in
	let players_in_fights = List.filter Model.Fight.is_fighting players in
	let players_and_combats = List.map 
		(fun player -> (
			Model.Fight.get_combat_info (
				Model.Fight.get_fight player
			), player)
		) players_in_fights in
	let all_combats = List.map fst players_and_combats in
	let combats = unique (!=) all_combats in
	let is_due = Model.Fight.is_due ~now in
	let combats = List.filter is_due combats in
		List.iter (fun combat -> 
			let player = List.assq combat players_and_combats in
				timeout ~player combat)
			combats


let init () =
	let frequency = 10.0 in
	let tick_fn = fun ~self ->
		handle_timed_out_fight_rounds ()
	in
	let dmn = Daemon.create ~frequency ~tick_fn in
		Daemon.start dmn
