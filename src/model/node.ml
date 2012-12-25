(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type _ connectivity =
	| Tree : 'mudobject	-> 'mudobject Container.container connectivity
	| Digraph : 'mudobject -> 'mudobject Digraph.digraph connectivity

let create : type s . s connectivity -> s = function
	| Digraph d -> Digraph.create d
	| Tree t -> Container.create t
