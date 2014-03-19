(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Utils.Ext

type 'a container = {
  thing: 'a;
  mutable parent: 'a container option;
  mutable children: 'a container list;
}

let create thing =
  { thing = thing;
    parent = None;
    children = [];
  }

let receive parent child =
  parent.children <- child :: parent.children

let eject parent child =
  ( match child.parent with
      | None -> failwith "Attempt to eject child with no parent from a parent"
      | Some p -> assert (List.memq child parent.children) );

  parent.children <- remove child parent.children

let remove_from parent child =
  eject parent child;
  child.parent <- None

let insert_into parent child =
  ( match child.parent with
      | None -> ()
      | Some oldParent -> remove_from oldParent child );

  receive parent child;
  child.parent <- Some parent

let children parent =
  parent.children

let contained i =
  i.thing

let parent child =
  child.parent
