(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


(*	let room_name = "Main Room", Name.Definite, Name.Singular in
	let room = Model.Create.create_room room_name ~desc:"A room.\n" in
*)

let test_grammar2 () =
	let obj_name = "Zach", Name.NoAdam, Name.Singular in
	let obj =  Model.Create.create_item obj_name ~desc:"Zach the player"
		~flags:[] ~stats:[] in 
		print_endline (Grammar.render ~actor:obj "%Av %vperf:log on.")

let test_grammar1 () =
	let obj_name = "dental floss", Name.Mass, Name.Singular in
	let obj =  Model.Create.create_item obj_name ~desc:"A bit of floss"
		~flags:[] ~stats:[] in 
		print_endline (Grammar.render "You %vperf:pick up %pu." ~patient:obj ~person:Grammar.Second)

let test_main () =
	print_endline "running tests";
	Lexicon.init ();
	test_grammar1 ();
	test_grammar2 ();
	flush_all ()

(*
let _ = test_main ()
*)
