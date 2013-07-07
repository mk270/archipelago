(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type connection_state =
	| NewConnection
	| UnAuthenticated
	| GetPass
	| ConfirmName of string
	| GetSex
	| NewPass1
	| NewPass2
	| LoggedIn of Model.mudobject
	| LoggedOut
	| Entering of string * int

type session = 
		{ 
			ss_socket : Socket.socket ;
			mutable ss_state : connection_state ;
			mutable ss_name : string option ;
			mutable ss_password : string option ;
		}  

let sockets = ref []
let player_sessions = ref []

let register_socket s sess =
	sockets := (s, sess) :: !sockets
	  
let deregister_socket s =
	sockets := List.remove_assq s !sockets

let remove_session_link ~player =
	player_sessions := List.remove_assq player !player_sessions

let add_session_link ~player ~session = 
	assert (not (List.mem_assq player !player_sessions));
	
	player_sessions := (player, session) :: !player_sessions

let current_players () = List.map fst !player_sessions

let current_players_and_sessions () = !player_sessions

let session_from_player ~player = 
	List.assq player !player_sessions

let emit sess data =
	Socket.sock_emit sess.ss_socket data 

let get_session s = 
	List.assq s !sockets

let pl_get_session sess =
	match sess.ss_state with
		| LoggedIn p -> p
		| _ -> raise Not_found

let emitl player line =
	try
		let sess = session_from_player ~player in
			emit sess (line ^ "\r\n")
	with Not_found -> () (* e.g., tried to talk to a monster *)
		(* FIXME: we should probably catch/suppress these *)

let get_state sess = sess.ss_state

let set_state session new_state = 
	( match session.ss_state with
		  | LoggedIn player -> remove_session_link ~player
		  | _ -> () );
	( match new_state with
		  | LoggedIn player -> add_session_link ~player ~session
		  | _ -> () );
	session.ss_state <- new_state

let set_name sess n = sess.ss_name <- Some n
let get_name sess = 
	match sess.ss_name with
		| Some n -> n
		| None -> failwith "No name set"

let set_password sess p = sess.ss_password <- Some p
let get_password sess = 
	match sess.ss_password with
		| Some p -> p
		| None -> failwith "No password set."

let new_session s = {
	ss_socket = s;
	ss_state = NewConnection;
	ss_name = None;
	ss_password = None;
}

let init_session sess =
	let new_socket = sess.ss_socket in
		register_socket new_socket sess;
		Socket.init new_socket

let new_connection l =
	let new_socket = Socket.accept_socket l in
	let sess = new_session new_socket in
		init_session sess;
		Some new_socket

let handle_read s =
	match Socket.is_listener s with
		| true -> new_connection s (* return new socket to multiplexer *)
		| false -> Socket.read s s; None

let end_session sess =
	deregister_socket sess.ss_socket;
	Socket.close sess.ss_socket
