(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Model

type connection_state =
  | NewConnection
  | UnAuthenticated
  | GetPass
  | ConfirmName of string
  | GetSex
  | NewPass1
  | NewPass2
  | LoggedIn of mudobject
  | LoggedOut
  | Entering of string * int

type session

val get_state : session -> connection_state
val set_state : session -> connection_state -> unit
val set_name : session -> string -> unit
val get_name : session -> string
val set_password : session -> string -> unit
val get_password : session -> string

val pl_get_session : session -> mudobject

val end_session : session -> unit
val get_session : Socket.socket -> session
val emit : session -> string -> unit
val emitl : mudobject -> string -> unit

val current_players : unit -> mudobject list
val current_players_and_sessions : unit -> (Model.mudobject * session) list

val new_connection : Socket.socket -> Socket.socket option
