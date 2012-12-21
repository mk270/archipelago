(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

let db_file = "../db/arch.db"
let port = 2500

let init_all () =
	World.init db_file;
	Commands.init ();
	Persona.init_spell_book ();
	Passwd.init ();
	Lexicon.init ();
	Random.self_init ();
	Atmosphere.init ();
	Fights.init ()

let fini_all () =
	Daemon.fini ();
	Lexicon.fini ();
	Passwd.fini ();
	Persona.fini ();
	Commands.fini ();
	World.fini ()

let main ~test_mode =
	if test_mode
	then Test_all.run ()
	else (
		init_all ();
		Server.run port;
		fini_all ()
	)

let parse_cmdline () =
	let test_mode = ref false in
	let args = ref [] in
	let speclist = [
		("--test", Arg.Unit (fun () -> test_mode := true), ": test mode");
	] in
	let usage = "Usage unavailable" in

	let collect arg = args := !args @ [arg] in
	let () = Arg.parse speclist collect usage in
	let test_mode = !test_mode in
		main ~test_mode
		
let () = 
	parse_cmdline ()
