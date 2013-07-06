(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Tcp_server
open Lwt

open Socket
(*
let callbacks = [
	Connection.check_logout;
	Monster.pump_events;
	Events.pump_all;
	Leaving.check_deaths;
]

let run port =
	let h = Connection.line_protocol in
	let l = Socket.create (Listener (port, h)) in
	let m = Multiplexer.create () in
	let register cb = Multiplexer.add_timeout_callback m (fun () -> cb ()) in
		Multiplexer.add_socket m l;
		List.iter register callbacks;
		Multiplexer.run m
*)
let io_loop connection_id input output =
	let respond = function
		| "help" -> [(connection_id, "no help available\r\n")]
		| "shout" ->
			let ids = Tcp_server.all_connection_ids () in
				List.map (fun id -> (id, "HI!\r\n")) ids
		| _ -> [(connection_id, "what?\r\n")]
	in

	Lwt_io.read_line input >|=
	respond >|=
	Tcp_server.enqueue_all

let cb connection_id input output = 
	Tcp_server.enqueue connection_id "Hello?\r\n";
    while_lwt true do io_loop connection_id input output done

let run port = 
	let sa = Unix.ADDR_INET (Unix.inet_addr_any, port) in
	let serv = Tcp_server.create sa cb in
		Lwt_main.run serv
