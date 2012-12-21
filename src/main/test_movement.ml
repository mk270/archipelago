(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open OUnit

open Name

let setup () = 
	World.init ()

let teardown () = 
	World.fini ()

(* create player
   insert into limbo
   try move up
   assert raises aperture closed
   open wormhole
   move up
   check player is at the pit *)

let assert_actor_in ~actor ~room =
	let cmp = (==) in
	let current_parent = Model.Tree.parent actor in
		assert_equal ~cmp current_parent room

let test_go_through_wormhole () = 
	let start_room = World.get_start_room () in
	let name = "Player" in
	let actor = Model.Create.create_player (name, NoAdam, Singular) in
	let room = World.get_a_room World.ThePit in
	let dir = (Direction.match_direction "up") in
		Model.Tree.insert_into ~recipient:start_room actor;

   		assert_actor_in ~actor ~room:start_room;
(*	    Verbs.go_dir (Direction.match_direction "up") ~actor *)

	let current_parent = Model.Tree.parent actor in
	let expected_portal = Model.Link.portal_in_direction ~src:current_parent ~dir in

		(try Model.Link.move_dir actor dir
		with 
			| Model.Portal_not_open observed_portal ->
				(if expected_portal != observed_portal
				then assert_failure "Crashed into wrong door"
				else ())
		);

		let _ = Model.Props.change_item_aperture_state expected_portal `Open in
			Model.Link.move_dir actor dir;
			assert_actor_in ~actor ~room
		   

let suite =
	"move-suite" >::: [
		"test_wormhole" >:: (bracket setup test_go_through_wormhole teardown);
	]

let run () = 
	run_test_tt_main suite

let main () =
	ignore(run ())
