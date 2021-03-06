(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Unix
open Networking

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

type socket = 
		{ 
			sock_port : int option ; 
			sock_socket : file_descr ;
			sock_peer_addr : sockaddr option ;
			mutable sock_closing : bool ;   
			sock_read_buffer : Buffer.t ;
			mutable sock_write_buffer : string ;
			mutable sock_session : session option ;
			sock_protocol : proto ;
			sock_listener : bool ;
		}
and session = 
		{ 
			socket : socket ;
			mutable state : connection_state ;
			mutable name : string option ;
			mutable password : string option ;
		}  
and proto = 
		{
			handle_init : socket -> unit ;
			handle_read : socket -> unit ;
		}

type socket_role = Listener of int * proto | Connection of file_descr

let player_sessions = ref []

let remove_session_link ~player =
	player_sessions := List.remove_assq player !player_sessions;
	Game.remove_current_player player

let add_session_link ~player ~session = 
	assert (not (List.mem_assq player !player_sessions));
	
	player_sessions := (player, session) :: !player_sessions;
	Game.add_current_player player

let current_players () = List.map fst !player_sessions

let current_players_and_sessions () = !player_sessions

let session_from_player ~player = 
	List.assq player !player_sessions

let pump_write s =
	let buf_size = String.length s.sock_write_buffer in
	let written = really_write s.sock_socket s.sock_write_buffer in
		s.sock_write_buffer <- String.sub s.sock_write_buffer written (buf_size - written)

let sock_emit s data =
	s.sock_write_buffer <- s.sock_write_buffer ^ data;
	pump_write s

let emit sess data =
	sock_emit sess.socket data 


let get_session s = 
	match s.sock_session with
      | None -> raise Not_found
      | Some ss -> ss

let pl_get_session sess =
	match sess.state with
		| LoggedIn p -> p
		| _ -> raise Not_found

let exhaust_input s =
	really_read s.sock_socket


let get_state sess = sess.state

let set_state session new_state = 
	( match session.state with
		  | LoggedIn player -> remove_session_link ~player
		  | _ -> () );
	( match new_state with
		  | LoggedIn player -> add_session_link ~player ~session
		  | _ -> () );
	session.state <- new_state

let set_name sess n = sess.name <- Some n
let get_name sess = 
	match sess.name with
		| Some n -> n
		| None -> failwith "No name set"

let set_password sess p = sess.password <- Some p
let get_password sess = 
	match sess.password with
		| Some p -> p
		| None -> failwith "No password set."

let emitl player line =
	try
		let sess = session_from_player ~player in
			emit sess (line ^ "\r\n")
	with Not_found -> () (* e.g., tried to talk to a monster *)
		(* FIXME: we should probably catch/suppress these *)

let dummy_file_descr = 
	let tmp = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o666 in
		Unix.close tmp;
		tmp
		
let dummy_handler s = ()

let dummy_protocol = {
	handle_init = dummy_handler ;
	handle_read = dummy_handler ;
}

let dummy_socket = {
	sock_port = None;
	sock_socket = dummy_file_descr ; 
	sock_peer_addr = None ;
	sock_closing = false ; 
	sock_read_buffer = Buffer.create 80 ; 
	sock_write_buffer = "" ; 
	sock_session = None ;
	sock_protocol = dummy_protocol ;
	sock_listener = false ;
}
  
let create_connection fd =
	{ dummy_socket with sock_socket = fd }
	  
let new_connection l =
	let new_fd, addr = accept l.sock_socket in
	let new_socket = { 
		dummy_socket with 
			sock_socket = new_fd ; 
			sock_peer_addr = Some addr ; 
			sock_protocol = l.sock_protocol;
	} in
	let sess = { 
		socket = new_socket ; 
		state = NewConnection ;
		name = None ;
		password = None ;
	} in
		set_nonblock new_socket.sock_socket;
		new_socket.sock_session <- Some sess;
		new_socket.sock_protocol.handle_init new_socket;
		Some new_socket
			
let create_listener p h =
	let s = Unix.socket PF_INET SOCK_STREAM 0 in
	let addr = ADDR_INET (inet_addr_any, p)    
	in
		set_nonblock s;
		setsockopt s SO_REUSEADDR true;
		bind s addr;
		listen s 5;
		{ 
			dummy_socket with 
				sock_port = Some p ; 
				sock_socket = s ; 
				sock_peer_addr = None ;
				sock_protocol = h ;
				sock_listener = true ;
		}

let create role = 
	match role with
		|  Listener (port, h) -> create_listener port h
		| Connection fd -> create_connection fd

let fd s =
	s.sock_socket

let handle_read s =
	match s.sock_listener with
		| true -> new_connection s (* return new socket to multiplexer *)
		| false -> s.sock_protocol.handle_read s ; None

let is_closing s =
	s.sock_closing

let close s =
	Unix.close s.sock_socket;
	s.sock_closing <- true

let end_session sess =
	close sess.socket

let rdbuf_append s data =
	Buffer.add_string s.sock_read_buffer data

let rdbuf_clear s =
	Buffer.clear s.sock_read_buffer

let rdbuf_set s data =
	Buffer.clear s.sock_read_buffer;
	Buffer.add_string s.sock_read_buffer data
  
let rdbuf_get s =
	Buffer.contents s.sock_read_buffer
