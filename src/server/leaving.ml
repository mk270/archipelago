(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

(*

  on disconnect:
    lose armour
    get out of vehicle
    lose fight
    drop everything
    kill any relevant daemons
    unbind

  on death:
    lose fight (qv)
    reward killer
    possibly lose life
    possibly disconnect
 
  lose life:
    if zero
      make tombstone
      become raisable

*)

let disconnect ~actor =
	Model.quit ~actor

let die ~actor =
	let event = Model.Death (actor, Model.Died_of "death") in
		Model.post_event event;
		disconnect ~actor

let check_deaths () = 
	let pp = Game.current_players () in
	let deaded = List.filter (fun p -> Model.Props.is_dead p) pp in
		List.iter (fun actor -> die ~actor) deaded
