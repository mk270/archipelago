(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


val get_verb_base : string -> string
val get_verb_past_simple : string -> string
val get_verb_past_participle : string -> string
val get_verb_pres_participle : string -> string
val get_verb_pres_3p : string -> string

val init : unit -> unit
val fini : unit -> unit
