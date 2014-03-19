(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Light

type light_msg = 
	| SwitchOn
	| SwitchOff
	| BlowOut

type light_fsm = (unit, light_msg, light) Utils.Fsm.fsm

exception Already_light of light
exception Invalid_light_change

let light_handler (state, input) = 
	match (state, input) with
		| Lit, BlowOut -> Lightable
		| _, BlowOut
		| Glowing, _ -> raise Invalid_light_change
		| Lit, SwitchOn
		| RobustlyLit, SwitchOn
		| Lightable, SwitchOff
		| RobustlyLightable, SwitchOff -> raise (Already_light state)
		| Lit, SwitchOff -> Lightable
		| RobustlyLit, SwitchOff -> RobustlyLightable
		| Lightable, SwitchOn -> Lit
		| RobustlyLightable, SwitchOn -> RobustlyLit

let light_handler (state, input) =
	(light_handler (state, input), ())

let create state =
	Utils.Fsm.create state light_handler
