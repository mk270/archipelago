(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type verb_forms = {
	vf_base : string;
	vf_past_simple : string;
	vf_past_participle : string;
	vf_pres_3p : string;
	vf_pres_participle : string;
}

let irregulars = [
	("have", "has", "having", "had", "had");
	("go", "goes", "going", "went", "gone");
	("log", "logs", "logging", "logged", "logged");
	("rise", "rises", "rising", "rose", "risen");
	("fall", "falls", "falling", "fell", "fallen");
	("arrive", "arrives", "arriving", "arrived", "arrived"); (* FIXME *)
	("drop", "drops", "dropping", "dropped", "dropped");
]

(* ##
type verb_schemes = 
	| E
	| IE
	| EE
	| S
	| Sibilant
	| VY
	| Y
	| VC
	| VStrongC
	| I
	| O
	| A
	| U
*)

(* ##
let matchers = [
	("ee", EE);
	("ie", IE);
	("e", E);
	("[^s]s", S);
	("\\(x\\|ch\\|sh\\|\\ss\\)", Sibilant);
	("[aeou]y", VY);
	("y", Y);
	("[aeoiu][bcdfgjklmnptvz]", VC);
	("[aeoiu][hqrw]", VStrongC);
	("i", I);
	("o", O);
	("a", A);
	("u", U);
]

let matchers = List.map (fun (pat, vs) -> (Str.regexp (pat ^ "$"), vs)) matchers
*)

let verbs = Hashtbl.create 50

let make_verb base =
	{
		vf_base = base;
		vf_past_simple = base ^ "ed";
		vf_past_participle = base ^ "ed";
		vf_pres_3p = base ^ "s";
		vf_pres_participle = base ^ "ing";
	}

let init () =
	List.iter (
		fun (base, pres3p, presp, pasts, pastp) ->
			Hashtbl.replace verbs base {
				vf_base = base;
				vf_past_simple = pasts;
				vf_past_participle = pastp;
				vf_pres_3p = pres3p;
				vf_pres_participle = presp;
			}
	) irregulars;
	()

let fini () =
	Hashtbl.clear verbs

let get_verb base =
	try Hashtbl.find verbs base
	with Not_found -> 
		let v = make_verb base in
			Hashtbl.replace verbs base v;
			v

let get_verb_base base =
	let v = get_verb base in
		v.vf_base

let get_verb_past_simple base =
	let v = get_verb base in
		v.vf_past_simple

let get_verb_past_participle base =
	let v = get_verb base in
		v.vf_past_participle

let get_verb_pres_participle base =
	let v = get_verb base in
		v.vf_pres_participle

let get_verb_pres_3p base =
	let v = get_verb base in
		v.vf_pres_3p
