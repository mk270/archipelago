(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type aperture_state = Open | Closed | Locked | Trapped
(* FIXME: no need for polymorphic variants here *)
type ap_message = [ `Shut | `Open | `Lock | `Unlock | `Trap | `Deactivate ]

type aperture_fsm = (string, ap_message, aperture_state) Utils.Fsm.fsm

exception Aperture_already of aperture_state
exception Trap_set_off
exception Not_permitted
exception Unknown_ap_transition

let ap_inp_handler (state, input) =
	match (state, input) with
	| Closed, `Shut 
	| Closed, `Unlock 
	| Open, `Open
	| Locked, `Shut 
	| Locked, `Lock -> raise (Aperture_already state)
	| Trapped, `Shut -> raise (Aperture_already Closed)
	| Trapped, `Deactivate -> (Closed, "You deactivate the trap on %pu.")
	| Trapped, _ -> raise Trap_set_off
	| _, `Deactivate -> (state, "You don't find a trap on %pu.")
	| Open, `Shut -> (Closed, "You shut %pu.")
	| Closed, `Open -> (Open, "You open %pu.")
	| Closed, `Lock -> (Locked, "You lock %pu.")
	| Locked, `Unlock -> (Closed, "You unlock %pu.")
	| Open, _
	| Locked, `Open
	| Locked, `Trap -> raise Not_permitted
	| Closed, `Trap -> (Trapped, "You set a trap on %pu.")
	| _ -> raise Unknown_ap_transition
