(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type priority = float

type 'a queue

exception Queue_is_empty
exception Comparand_too_great

val empty : 'a queue
val insert : 'a queue -> priority -> 'a -> 'a queue
val extract : 'a queue -> priority * 'a * 'a queue
val extract_if_lesser : comparand : priority -> 'a queue
	-> priority * 'a * 'a queue

val iter : ?comparand : priority -> (priority -> 'a -> unit) -> 'a queue -> 'a queue
val top_priority : 'a queue -> priority
