(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type t

val create : flags : string list -> stats : (string * string) list -> t
val value : t -> int
val set_pitted : t -> unit

val is_lit : t -> bool
val light_change : t -> Light_fsm.light_msg -> unit
val vehicularity : t -> Vehicle.vehicle option
