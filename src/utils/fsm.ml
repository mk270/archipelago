(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type ('response, 'message, 'state) fsm = { 
	mutable fsm_state : 'state ; 
	fsm_input_handler : ('state * 'message) -> ('state * 'response) ;
}

let create state input_handler = {
	fsm_state = state;
	fsm_input_handler = input_handler;
}

let send fsm msg =
	let new_state, output = fsm.fsm_input_handler (fsm.fsm_state, msg) in
		fsm.fsm_state <- new_state;
		output
