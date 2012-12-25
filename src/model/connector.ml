(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type connv =
	| VTree
	| VDigraph

type ('a, 'b)  connector = {
	thing : 'a;
	trees : ('b * 'a Container.container) list;
	digraphs : ('b * 'a Digraph.digraph) list;
}

let create networks thing =
	{
		thing = thing;
		trees = 
			List.map (fun (tag, ty) -> (tag, Container.create thing))
			(List.filter (fun (tag, ty) -> ty = VTree) networks);
		digraphs = 
			List.map (fun (tag, ty) -> (tag, Digraph.create thing))
			(List.filter (fun (tag, ty) -> ty = VDigraph) networks);
	}
