(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Utils
open Unix
open Socket

type multiplexer = { 
(*	mutable ll : socket list ;  *)
	mutable readfds : file_descr list ;
	mutable writefds : file_descr list ;
	mutable exceptfds : file_descr list ;
	timeout : float ;
	mutable running : bool ;
	mutable switchboard : (file_descr, socket) Hashtbl.t ;
	mutable callbacks : (unit -> unit) list ;
}

let create () = 
{ 
(*	ll = [] ;  *)
	running = false ;
	readfds = [] ;
	writefds = [] ;
	exceptfds = [] ;
	timeout = 9.9 ;
	switchboard = Hashtbl.create 10 ;
	callbacks = [] ;
}

let add_socket m l = 
	let fd = Socket.fd l
	in
		m.readfds <- fd :: m.readfds;
		(*    m.writefds <- fd :: m.writefds;*)
		m.exceptfds <- fd :: m.exceptfds;
		Hashtbl.replace m.switchboard fd l

let remove_socket m l =
	let fd = Socket.fd l
	in
		m.readfds <- remove fd m.readfds;
		m.writefds <- remove fd m.writefds;
		m.exceptfds <- remove fd m.exceptfds;
		Hashtbl.remove m.switchboard fd

let add_timeout_callback m thunk =
	m.callbacks <- thunk :: m.callbacks
	
let start m =
	m.running <- true

let stop m =
	m.running <- false

let timeout m =
	let fudge = 1.0 /. 1000000.0 in
		try
			let next_event = Workqueue.top_priority () +. fudge in
			let till_next = next_event -. Unix.gettimeofday () in
				min m.timeout till_next
		with Prioqueue.Queue_is_empty -> m.timeout

let poll m =
	let timeout = timeout m in
		select m.readfds m.writefds m.exceptfds timeout

let poll_readable m =
	let r, _, _ = poll m in
		r

let handle_read m fd =
	let l = Hashtbl.find m.switchboard fd in
	let read_result =  handle_read l in
		match read_result with
			| None -> ()
			| Some new_socket -> add_socket m new_socket

let run_callback thunk =
	try thunk ()
	with e -> Utils.guard_exception e

let pump m =
	let handle_read' = handle_read m
	in
	let rm_socket _ s =
		if is_closing s
		then remove_socket m s
	in
		Hashtbl.iter rm_socket m.switchboard;
		List.iter handle_read' (poll_readable m);
		Workqueue.pump_till_current ();
		List.iter run_callback m.callbacks;
		(* FIXME *)
		(* need to go through sockets with non-zero write_buffers and
		   give them a push - if they're in this state, it means that
		   they tried to write so much data that they got EAGAIN, and
		   we (the multiplexer, rather than the socket)
		   need to poll() to know when to retry *)
		(* iter handle active *)
		()

(*
let pump m = 
	try pump m
	with e ->
		let error = Printexc.to_string e in
			Printf.printf "Exception: %s\nBacktrace:\n''\n\n" error;
			flush_all ()
			*)

let close_sockets m = 
	(* I'm not sure if Hashtbl is re-entrant, so take no chances *)
	let tmp = Hashtbl.create 5 in
		Hashtbl.iter (fun k v -> Hashtbl.replace tmp k v) m.switchboard;
		Hashtbl.iter (fun k v -> 
			(try Unix.close k
			 with _ -> ());
			remove_socket m v) tmp

let run m =
	let rec keep_running m =
		match m.running with
			| true -> 
				pump m;
				(if (Reset.do_shutdown ())
				 then stop m);
				keep_running m
			| false -> ()
	in
		start m;
		keep_running m;
		close_sockets m
