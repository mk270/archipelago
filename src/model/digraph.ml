(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Utils.Ext

type 'a digraph = {
  thing: 'a;
  mutable successors: 'a digraph list;
  mutable predecessors: 'a digraph list;
}

let create thing =
  { thing = thing;
    successors = [];
    predecessors = [];
  }

let receive dst src =
  dst.predecessors <- src :: dst.predecessors

let eject dst src =
  assert (List.memq src dst.predecessors);
  dst.predecessors <- remove src dst.predecessors

let remove_arc ~src ~dst =
  assert (List.memq src dst.predecessors);
  eject dst src;
  src.successors <- remove src src.successors

let add_arc ~src ~dst =
  receive dst src;
  src.successors <- dst :: src.successors

let inbound i =
  i.predecessors

let contained i =
  i.thing

let outbound i =
  i.successors
