(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type direction

val get_compass : unit -> direction list
val string_of_direction : direction -> string
val whence : direction -> string
val whither : direction -> string
val direction_of_int : int -> direction
val int_of_direction : direction -> int
val match_direction : string -> direction

val create_direction : 
	dir_name : string ->
	whence : string ->
	whither : string ->
	dir_number : int ->
	direction

val clear : unit -> unit
