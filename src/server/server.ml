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

let callbacks = [
	Connection.check_logout;
	Monster.pump_events;
	Events.pump_all;
	Leaving.check_deaths;
]

let run port =
	let h = Connection.line_protocol in
	let l = Socket.create (Socket.Listener (port, h)) in
	let m = Multiplexer.create () in
	let register cb = Multiplexer.add_timeout_callback m (fun () -> cb ()) in
		Multiplexer.add_socket m l;
		List.iter register callbacks;
		Multiplexer.run m

(*let io_loop connection_id input output =
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

let start_fn connection_id =
	Tcp_server.enqueue connection_id "Hello?\r\n"

let _cb connection_id input output init_fn read_fn =
	init_fn connection_id;
	while_lwt true do read_fn connection_id input output done

let cb connection_id input output =
	_cb connection_id input output start_fn io_loop

let run port = 
	let sa = Unix.ADDR_INET (Unix.inet_addr_any, port) in
	let serv = Tcp_server.create sa cb in
		Lwt_main.run serv
*)
