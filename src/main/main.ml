(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

let default_db_file = "../db/arch.db"
let default_port = 2500

let init_all db_file =
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

let main ~test_mode port db_file =
	if test_mode
	then Test_all.run ()
	else (
		init_all db_file;
		Server.run port;
		fini_all ()
	)

let parse_cmdline () =
	let test_mode = ref false
	and port = ref default_port
	and db_file = ref default_db_file in
	let args = ref [] in
	let speclist = [
		("--port", Arg.Int  (fun i -> port := i), ": set TCP listener port");
		("--db",   Arg.Set_string db_file, ": set sqlite DB file");
		("--test", Arg.Unit (fun () -> test_mode := true), ": test mode");
	] in
	let usage = "Usage unavailable" in

	let collect arg = args := !args @ [arg] in
	let () = Arg.parse speclist collect usage in
	let test_mode = !test_mode in
		main ~test_mode !port !db_file
		
let () = 
	parse_cmdline ()
