(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type 'a container

val create : 'a -> 'a container

(*
val receive :    'a container -> 'a container -> unit
val eject :      'a container -> 'a container -> unit
*)
val remove_from : 'a container -> 'a container -> unit
val insert_into : 'a container -> 'a container -> unit
val children :   'a container -> 'a container list
val parent :     'a container -> 'a container option
val contained :  'a container -> 'a
