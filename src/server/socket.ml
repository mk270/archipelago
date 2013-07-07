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

type socket = 
		{ 
			sock_port : int option ; 
			sock_socket : file_descr ;
			sock_peer_addr : sockaddr option ;
			mutable sock_closing : bool ;   
			sock_read_buffer : Buffer.t ;
			mutable sock_write_buffer : string ;
			sock_protocol : proto ;
			sock_listener : bool ;
		}
and proto = 
		{
			handle_init : socket -> unit ;
			handle_read : socket -> unit ;
		}

type socket_role = Listener of int * proto | Connection of file_descr

let init_handler s = s.sock_protocol.handle_init

let init s =
	set_nonblock s.sock_socket;
	init_handler s s (* seems redundant *)

let pump_write s =
	let buf_size = String.length s.sock_write_buffer in
	let written = really_write s.sock_socket s.sock_write_buffer in
		s.sock_write_buffer <- String.sub s.sock_write_buffer written (buf_size - written)

let sock_emit s data =
	s.sock_write_buffer <- s.sock_write_buffer ^ data;
	pump_write s

let exhaust_input s =
	really_read s.sock_socket

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
	sock_protocol = dummy_protocol ;
	sock_listener = false ;
}

let accept_socket l =
	let new_fd, addr = accept l.sock_socket in
		{ 
			dummy_socket with 
				sock_socket = new_fd ; 
				sock_peer_addr = Some addr ; 
				sock_protocol = l.sock_protocol;
		} 
  
let create_connection fd =
	{ dummy_socket with sock_socket = fd }
			
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

let is_closing s =
	s.sock_closing

let close s =
	Unix.close s.sock_socket;
	s.sock_closing <- true

let rdbuf_append s data =
	Buffer.add_string s.sock_read_buffer data

let rdbuf_clear s =
	Buffer.clear s.sock_read_buffer

let rdbuf_set s data =
	Buffer.clear s.sock_read_buffer;
	Buffer.add_string s.sock_read_buffer data
  
let rdbuf_get s =
	Buffer.contents s.sock_read_buffer

let is_listener s = s.sock_listener

let read s = s.sock_protocol.handle_read
