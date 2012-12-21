(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Name


exception Missing_grammar_argument
exception Malformed_verb_spec
exception Unknown_tense
exception Unimplemented

type tense_aspect =
	| Present
	| Perfect

type person = 
	| Third
	| Second
	| First

(* ##
type mood =
	| Indicative
	| Subjunctive
*)

type verb =
	| Be
	| PastPreterite of string
	| Generic of string

let verb_of_string s = 
	match s with
		| "be" -> Be
		| "can"
		| "shall"
		| "will"
		| "must"
		| "may"
		| "dare" -> PastPreterite s
		| _ -> Generic s

type verb_form = 
	| Base of string
	| SimplePast of string
	| PastParticiple of string
	| PresentParticiple of string
	| Present3P of string

let handle_vf = function
	| Base s -> Lexicon.get_verb_base s
	| SimplePast s -> Lexicon.get_verb_past_simple s
	| PastParticiple s -> Lexicon.get_verb_past_participle s
	| PresentParticiple s -> Lexicon.get_verb_pres_participle s
	| Present3P s -> Lexicon.get_verb_pres_3p s

let number_of_subject s = Singular

(* FIXME: should recurse to do "have", etc *)

let conjugate_verb ~verb ~tense ~subject ~person =
	let number = number_of_subject subject in
		match verb, tense, number, person with
			| Be, Present, Singular, First -> "am"
			| Be, Present, Singular, Third -> "is"
			| Be, Present, _, _ -> "are"
			| Be, Perfect, Singular, First
			| Be, Perfect, Singular, Third -> "has been"
			| Be, Perfect, _, _ -> "have been"
			| PastPreterite s, Present, _, _ -> s
			| PastPreterite s, _, _, _ -> raise Unimplemented
			| Generic s, Present, Singular, Third -> handle_vf (Present3P s)
			| Generic s, Present, _, _ -> handle_vf (Base s)
			| Generic s, Perfect, Singular, Third -> 
				  handle_vf (Present3P "have") ^ " " ^ handle_vf (PastParticiple s)
			| Generic s, Perfect, _, _ ->
				  handle_vf (Base "have") ^ " " ^ handle_vf (PastParticiple s)

let tense_of_string = function
	| "vpres" -> Present
	| "vperf" -> Perfect
	| _ -> raise Unknown_tense

let render_verb ~actor ~person ~tense s =
	let r = Str.regexp "%\\(.+\\):\\(.*\\)" in
		match (Str.string_match r s 0) with
			| false -> raise Malformed_verb_spec
			| true -> 
				  try
					  let verb_type = Str.matched_group 1 s in
					  let tense = match tense with
						  | Some t -> t
						  | None -> tense_of_string verb_type in
					  let base = Str.matched_group 2 s in
					  let verb = verb_of_string base in
						  conjugate_verb ~verb ~tense ~subject:actor ~person
				  with Invalid_argument _ -> raise Malformed_verb_spec

let check_capitalisation code =
	assert (String.length code > 2);

	let code' = String.sub code 1 (String.length code - 1) in
		(code' = String.capitalize code', String.lowercase code)

let render_placeholder ~actor ~patient ~person ~tense code =
	let cap, code = check_capitalisation code in
	let res = 
		match code, actor, patient, person with
			| "%av", Some a, _, Second -> "you"
			| "%au", Some a, _, Second -> "you"
(*			| "%pv", Some a, _, Third -> "you"
			| "%pu", Some a, _, Third -> "you"*)
			| "%av", Some a, _, _ -> Model.Props.get_vague_name a
			| "%au", Some a, _, _ -> Model.Props.get_unvague_name a
			| "%pv", _, Some p, _ -> Model.Props.get_vague_name p
			| "%pu", _, Some p, _ -> Model.Props.get_unvague_name p
			| "%av", None, _, _ 
			| "%au", None, _, _
			| "%pv", _, None, _
			| "%pu", _, None, _ -> raise Missing_grammar_argument
			| s, _, _, _ -> render_verb ~actor ~person ~tense s
	in
		match cap with
			| false -> res
			| true -> String.capitalize res

let render_elt ~actor ~patient ~person ~tense = function
	| Str.Text s -> s
	| Str.Delim delim -> render_placeholder ~actor ~patient ~person ~tense delim

let analyse_format fmt =
	let r = Str.regexp "%\\([a-zA-Z:]+\\)\\b" in
		Str.full_split r fmt

let render ?actor ?patient ?person ?tense fmt =
	let person = match person with
		| None -> Third
		| Some p -> p
	in
	let render_elt elt =
		render_elt ~actor ~patient ~person ~tense elt
	in
		String.concat "" (List.map render_elt (analyse_format fmt))


let () = ignore ( SimplePast "dummy", PresentParticiple "dummy" )
