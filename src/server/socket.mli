(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Unix
open Model

type 
  socket
and
  proto = 
    {
      handle_init : socket -> unit ;
      handle_read : socket -> unit ;
    }

type socket_role = Listener of int * proto | Connection of file_descr

val create : socket_role -> socket

val fd : socket -> file_descr
(* val handle_read : socket -> socket option *)
val is_closing : socket -> bool
val exhaust_input : socket -> string
val close : socket -> unit
val rdbuf_append : socket -> string -> unit
val rdbuf_clear : socket -> unit
val rdbuf_get : socket -> string
val rdbuf_set : socket -> string -> unit

val sock_emit : socket -> string -> unit

val init : socket -> unit
val accept_socket : socket -> socket
val is_listener : socket -> bool
val read : socket -> (socket -> unit)
