(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type player_business =
	| JuryService
	| Duelling

type nqmode =
	{ wiz_mode : bool ;
	  busy : player_business option ; }

type quest = string

type mode =
	| Quest of quest
	| NonQuest of nqmode

type gladness =
	| Glad
	| Jethead

type mode_msg =
	| EnterWizmode
	| ExitWizmode
	| StartQuest of quest
	| EndQuest
	| StartDuel
	| EndDuel
	| StartJury
	| EndJury

type mode_fsm = (unit, mode_msg, mode) Utils.Fsm.fsm

exception Playermode_already of mode
exception Invalid_modechange

type t = {
	mutable player_mode : mode_fsm ;
	invisible : bool ;
	morphed : bool ;
	glad : gladness option ;
	armour : int;
	mutable night_mode : bool ;
	mutable dead : bool;
}

let default_mode = NonQuest { wiz_mode = false ; busy = None; }


let mode_handler (state, input) =
	match (state, input) with
		| NonQuest { busy = Some JuryService; _ }, StartJury
		| NonQuest { busy = Some Duelling; _ }, StartDuel
		| NonQuest { wiz_mode = true; _ }, EnterWizmode
		| Quest _, StartQuest _ -> raise (Playermode_already state)
		| Quest _, EndQuest -> default_mode
		| NonQuest { busy = Some Duelling; _ }, StartJury
		| NonQuest { busy = Some Duelling; _ }, EndJury
		| NonQuest { busy = Some JuryService; _ }, StartDuel
		| NonQuest { busy = Some JuryService; _ }, EndDuel
		| NonQuest { busy = Some JuryService; _ }, StartQuest _
		| NonQuest { busy = Some JuryService; _ }, EndQuest ->
			raise Invalid_modechange
		| (NonQuest m, EnterWizmode) when m.wiz_mode = false -> 
			NonQuest { m with wiz_mode = true }
		| NonQuest _, _ -> failwith "player mode change not implemented"
		| Quest _, _ ->  raise Invalid_modechange
			
			
let mode_handler (state, input) = (mode_handler (state, input), ())

let create () =
	let new_mode = Utils.Fsm.create default_mode mode_handler in
		{
			player_mode = new_mode;
			invisible = false;
			morphed = false;
			glad = None;
			armour = 1;
			night_mode = false;
			dead = false;
		}

(* currently this stuff totally bypasses the fsm machinery *)

let in_wizmode ps =
	match ps.player_mode.Utils.Fsm.fsm_state with
		| NonQuest m -> m.wiz_mode = true
		| _ -> false

let in_nightmode ps = ps.night_mode

let in_questmode ps =
	match ps.player_mode.Utils.Fsm.fsm_state with
		| Quest _ -> true
		| _ -> false

(*
let set_wizmode ps state =
	match ps.player_mode.Fsm.fsm_state with
		| Quest _ -> failwith "not allowed on a quest" (* FIXME raise *)
		| NonQuest m -> ps.player_mode.Fsm.fsm_state <- NonQuest { m with wiz_mode = state }
						  *)
let set_wizmode ps = function
	| true -> Utils.Fsm.send ps.player_mode EnterWizmode
	| false -> Utils.Fsm.send ps.player_mode ExitWizmode


let get_prompt ps =
	match ps.player_mode.Utils.Fsm.fsm_state with
		| NonQuest nq -> (
			  match nq.wiz_mode with
				  | true -> "* "
				  | false -> "> "
		  )
		| Quest _ -> "# "			  

let set_quest ps quest =
	Utils.Fsm.send ps.player_mode (StartQuest quest)

let unset_quest ps =
	Utils.Fsm.send ps.player_mode EndQuest

let set_dead ps state =
	ps.dead <- state

let is_dead ps =
	ps.dead

let _ =
	ignore 
		(
			[JuryService; Duelling;],
			[Quest "dummy"],
			[Glad; Jethead;],
			[StartDuel; EndDuel;],
			[StartJury; EndJury;]
		)
