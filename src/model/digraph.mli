(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type 'a digraph

val create : 'a -> 'a digraph

val remove_arc : src : 'a digraph -> dst : 'a digraph -> unit
val add_arc :    src : 'a digraph -> dst : 'a digraph -> unit

val inbound :    'a digraph -> 'a digraph list
val outbound :   'a digraph -> 'a digraph list
val contained :  'a digraph -> 'a
