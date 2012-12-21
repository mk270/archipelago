(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type persona
type stat
type level
type restriction

type spell_failure =
	| No_such_spell
	| Spell_not_available
	| Restricted of restriction
	| Insufficient_magic_strength

exception Spell_failed of spell_failure

val create : unit -> persona
val init_spell_book : unit -> unit

val stat_incr : persona -> stat -> int -> unit
val stat_get  : persona -> stat -> int
val get_stat_name : stat -> string

val is_necro : persona -> bool
val is_wizard : persona -> bool
val set_guild : persona -> guild : Guild.t -> unit
val get_guild : persona -> Guild.t

val score : stat
val quests_done : stat
val gold : stat
val strength : stat
val max_strength : stat

val magic_strength : stat
val sin : stat
val hoopiness : stat
val fright : stat
val intelligence : stat
val dexterity : stat
val skill : stat
val size : stat
val capacity : stat
val debt : stat
val armour : stat
val lives : stat
val max_magic : stat


val get_level : persona -> level

val string_of_level : level -> string

val use_spell : persona : persona -> wizmode : bool -> spell_name : string -> unit
val buy_spell : persona : persona -> spell_name : string -> unit

val fini : unit -> unit
