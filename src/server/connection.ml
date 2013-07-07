(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Game_protocol
open Socket

let delim_crlf = Str.regexp "\r\n"

let handle_lines s =
	let lines = Str.split_delim delim_crlf (Socket.rdbuf_get s) in
	let rec get_lines ll =
		match ll with
			| [] -> Socket.rdbuf_clear s
			| [last] -> Socket.rdbuf_set s last
			| hd :: tl -> 
				  Game_protocol.dispatch (NewLine (s, hd));
				  get_lines tl
	in
		get_lines lines

let handle_data s data =
	Socket.rdbuf_append s data;
	handle_lines s

let handle_peer_hangup s =
	print_endline "socket hangup detected."; flush_all ();
	Game_protocol.dispatch (Hangup s)

let check_logout () =
	List.iter (fun (pl, sess) ->
		if Model.Props.get_logout pl 
		then Game_protocol.dispatch (Logout (sess, pl))
	) (Session.current_players_and_sessions ())
	
let new_input s =
	let data = exhaust_input s
	in
		(
			if String.length data = 0
			then handle_peer_hangup s
			else handle_data s data
		)

(* FIXME: what about forcible logouts? *)

let transition_state = Game_protocol.transition_state

(* ## !?
let hangup s = 
	print_endline "socket hang up. unhandled! please fix."; flush_all ();
	()
*)

let init s =
	let sess = Session.get_session s in
		transition_state sess Session.NewConnection

let line_protocol =
	{
		handle_init = init ;
		handle_read = new_input ;
	}
