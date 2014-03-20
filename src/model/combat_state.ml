(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

(* When trying to reimplement the Island combat system, I concluded that
   the source code made no sense unless the moves were synchronised, but
   I couldn't find the synchronisation mechanism.

   The code contained a comment to the effect that it was an implementation
   of the Lost Worlds (tm) combat system; I hadn't heard of this, and I
   don't expect it was well documented on the web in the 1990s. At the
   time of reimplementation (2011), there was soom good information available,
   including actual scans of the books involved. They looked like the
   output of the old "choices" command on Island! In Lost Worlds, combat
   is *inherently* synchronised: the game mechanic requires that both
   parties make their moves simultaneously. 

   Island however ping ponged tokens back and forth between the parties, 
   and you could get several moves in to your opponent's one if you typed 
   quickly enough. Now that I knew the original mechanic didn't work this
   way, I assumed it was just a deficiency of Island, and resolved to
   implement it the original way.

   As luck would have it, a few days later, for the first time in years,
   I delved into an old backup, looking for something else, and completely
   by chance came across the email below. I'd completely forgotten about
   its existence, and hadn't even understood it at the time I received it.
*)

(*
From: dawes@qucis.queensu.ca (Robin Dawes)
Date: Fri, 15 Nov 1996 11:04:23 -0500
To: martin@cam.sri.com
Subject: Re: Island's future

When contemplating a rewrite, please consider my original request that
the combat moves be synchronized.  This is neither conceptually nor
practically difficult.  The whole combat system, as I wrote it, was
predicated on synchronization of moves - the wonder of it is that under
the kludges added to de-synchronize, it works as well as it does.

[snip]

Robin
*)

type combatant_position = 
		 Engaged | Disengaged 
type combatant_move =
		 Scissors | Paper | Stone | Engage | Disengage 

type combatant_id =
	| Initiator
	| Responder

type combatant = combatant_position * combatant_move option

(* the one who initiated the attack gets to be the fst member *)

type combatant_pair = {
	init_cmbtnt : combatant;
	resp_cmbtnt : combatant;
}
(* type combatant_pair = combatant * combatant *)

type combat_round_result =
	 Unscathed | Hit of int | Round_incomplete 

type combat_round_result_pair = combat_round_result * combat_round_result

type combat_message =
	| Move of combatant_move * combatant_id
	| Timeout

type combat_fsm = 
		(combat_round_result_pair, combat_message, combatant_pair) Utils.Fsm.fsm

exception Already_moved
exception Invalid_combat_move
exception Invalid_combat_state

let create_combatant_pair () =
	{ init_cmbtnt = (Disengaged, None);
	  resp_cmbtnt = (Disengaged, None);
	}

let move_of_string = function
	| "scissors" -> Scissors
	| "paper" -> Paper
	| "stone" -> Stone
	| "engage" -> Engage
	| "disengage" -> Disengage
	| _ -> raise Not_found

let string_of_response = function
	| Unscathed -> "unscathed"
	| Hit _ -> "injured"
	| Round_incomplete -> "in progress"

let is_attack = function
	| Scissors
	| Paper
	| Stone -> true
	| _ -> false

(* ## *)
(*
let is_engaged state =
	match fst state with
		| Engaged -> true
		| _ -> false
*)

let new_position = function
	| Engaged,    Disengage -> Disengaged
	| Disengaged, Engage    -> Engaged
	| Engaged,    _          -> Engaged
	| Disengaged, _          -> Disengaged
(*	| _ -> failwith "combat position logic error" *)
	
let resolve_attack = function
	| a, b when a = b   -> (Unscathed, Unscathed)
	| Scissors, Stone -> (Hit 1, Unscathed)
	| Scissors, Paper -> (Unscathed, Hit 1)
	| Paper, Stone    -> (Unscathed, Hit 1)
	| Paper, Scissors -> (Hit 1, Unscathed)
	| Stone, Scissors -> (Unscathed, Hit 1)
	| Stone, Paper    -> (Hit 1, Unscathed)
	| _ -> failwith "combat combination logic error"

let resolve_round : (move : combatant_move -> 
					 position : combatant_position ->
					 opponent : combatant ->
					 combatant_pair * combat_round_result_pair) =
fun  ~move ~position ~opponent ->
	let opp_position, opp_move = opponent in
	let opp_move = match opp_move with 
		| Some mv -> mv 
		| None -> failwith "combat logic error" 
	in
	let attacking = is_attack move in
	let opp_attacking = is_attack opp_move in
	let results = 
		match attacking, opp_attacking with
			| true, true -> resolve_attack (move, opp_move)
			| false, false -> (Unscathed, Unscathed)
			| false, true -> (Hit 2, Unscathed)
			| true, false -> (Unscathed, Hit 2)
	in
		(* (((new_position (position, move), None),
		  (new_position (opp_position, opp_move), None)),
		 results) *)
		( {
			init_cmbtnt = (new_position (position, move), None);
			resp_cmbtnt = (new_position (opp_position, opp_move), None);
			}, results )
			


let assert_move_valid = function
	| (_, Some _), _ -> raise Already_moved
	| (pos, None), move ->
		match (pos, move) with
			| Disengaged, Engage -> ()
			| Disengaged, _ -> raise Invalid_combat_move
			| Engaged, Engage -> raise Invalid_combat_move
			| Engaged, move -> ()
(*			| _ -> raise Invalid_combat_state *)

let has_already_moved cmb =
	match (snd cmb) with
		| Some _ -> true
		| None -> false

let try_resolve_round ~move ~combatant ~opponent =
	let position = fst combatant in
		assert_move_valid (combatant, move);

		if not (has_already_moved opponent)
		then (
			({ init_cmbtnt = (position, Some move);
			   resp_cmbtnt = opponent; }), 
			(Round_incomplete, Round_incomplete))
		else resolve_round ~move ~position ~opponent

let try_resolve_timeout ~move ~combatant ~opponent =
	(
		{init_cmbtnt = ((new_position (fst combatant, move)), None);
		 resp_cmbtnt = opponent;
		}, 
		(Unscathed, if is_attack move then Hit 1 else Unscathed)
	)

let transpose_combatants cmbtnt_pair = function
	| Initiator -> cmbtnt_pair
	| Responder -> {
		init_cmbtnt = cmbtnt_pair.resp_cmbtnt;
		resp_cmbtnt = cmbtnt_pair.init_cmbtnt;
	}

let transpose_results (res1, res2) = function
	| Initiator -> (res1, res2)
	| Responder -> (res2, res1)

let combat_move_handler' ~handler (state, input) =
	let move, id = input in
	let combatants = transpose_combatants state id in
	let combatant = combatants.init_cmbtnt in
	let opponent = combatants.resp_cmbtnt in
	let (cmbtnt_pair, (comb_result, opp_result)) =
		handler ~move ~combatant ~opponent
	in
		((transpose_combatants cmbtnt_pair id), 
		 (transpose_results
			  (comb_result, opp_result) id))

let combat_move_handler (state, input) =
	let handler = try_resolve_round in
		combat_move_handler' ~handler (state, input)

let timeout_move_handler (state, input) =
	let handler = try_resolve_timeout in
		combat_move_handler' ~handler (state, input)

let timeout_handler state =
	(* if they're both idle, return the same state + both uninjured *)
	let combatant = state.init_cmbtnt in
	let opponent = state.resp_cmbtnt in
		match (snd combatant), (snd opponent) with
			| None, None -> (state, (Unscathed, Unscathed))
				(* FIXME: this stuff would benefit from the
				   transpose_* functions above *)
			| Some move, None ->
				timeout_move_handler (state, (move, Initiator))
			| None, Some move -> 
				timeout_move_handler (state, (move, Responder))
			| Some _, Some _ -> failwith "timeout even though both combatants have moved"

let combat_input_handler (state, input) =
	match input with
		| Timeout -> timeout_handler state
		| Move (mv, id) -> combat_move_handler (state, (mv, id))


