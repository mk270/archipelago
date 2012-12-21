(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type t

val create : unit -> t

val in_wizmode : t -> bool
val in_nightmode : t -> bool
val in_questmode : t -> bool
val set_wizmode : t -> bool -> unit

val get_prompt : t -> string
val set_quest : t -> string -> unit
val unset_quest : t -> unit

val set_dead : t -> bool -> unit
val is_dead : t -> bool

