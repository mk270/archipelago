(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

val parse_line : Model.MudobjectSet.elt -> string -> unit

val init : unit -> unit
val fini : unit -> unit

val help : actor : Model.mudobject -> unit
