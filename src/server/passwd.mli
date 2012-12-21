(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


val sensible_password : string -> bool
val player_name_known : string -> bool
val password_matches : string -> string -> bool
val sensible_name : string -> bool
val normalise_name : string -> string
val name_too_similar : name : string -> bool
val init : unit -> unit
val fini : unit -> unit
