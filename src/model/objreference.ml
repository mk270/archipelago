(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type objreference =
    | Containment
    | Key
    | Vulnerability
    | Drive
    | IsBody
    | BodyPossesses
    | BodyWears
    | CombatOpponent
    | LinkToRoom of Direction.direction
    | LinkPortal of Direction.direction
    | LinkRestriction of Direction.direction