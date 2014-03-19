(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type ap_message = [ `Shut | `Open | `Lock | `Unlock | `Trap | `Deactivate ]
type aperture_state = Open | Closed | Locked | Trapped

type aperture_fsm = (string, ap_message, aperture_state) Utils.Fsm.fsm

val ap_inp_handler : (aperture_state * ap_message) -> (aperture_state * string)
