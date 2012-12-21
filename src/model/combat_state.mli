(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type combatant_move 

type combatant_id =
	| Initiator
	| Responder

type combatant

(* the one who initiated the attack gets to be the fst member *)
type combatant_pair = {
	init_cmbtnt : combatant;
	resp_cmbtnt : combatant;
}

type combat_round_result =
	| Unscathed | Hit of int | Round_incomplete 

type combat_round_result_pair = combat_round_result * combat_round_result

type combat_message =
	| Move of combatant_move * combatant_id
	| Timeout

type combat_fsm = 
		(combat_round_result_pair, combat_message, combatant_pair) Fsm.fsm


exception Already_moved
exception Invalid_combat_move
exception Invalid_combat_state

val create_combatant_pair : unit -> combatant_pair
val move_of_string : string -> combatant_move
val combat_input_handler : combatant_pair * combat_message ->
	combatant_pair * combat_round_result_pair

val string_of_response : combat_round_result -> string
