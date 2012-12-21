(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


(*
  score
  lives
  gold
  level (should be a dynamic function)
  questsdone

*)

type level = {
	lv_level : int;
}

type stat = {
	stat_name : string;
	stat_prio : int; (* for sorting *)
	stat_max : (level : level -> guild : Guild.t -> int);
	stat_default : int;
}

type restriction = {
	restr_min_level : level option;
	restr_wiz_mode : bool;
}

type spell = { 
	spell_name : string;
	spell_restriction : restriction;
	spell_ms_cost : int;
	spell_gp_cost : int;
}

module SpellSet = Set.Make (
	struct
		type t = spell
		let compare x y = 
			compare x.spell_name y.spell_name
	end
)

type persona = {
	pers_age : int ;
	mutable pers_guild : Guild.t ;
	mutable pers_spells : SpellSet.t ;
	mutable pers_stats : (stat, int) Hashtbl.t ;
}

type spell_failure =
	| No_such_spell
	| Spell_not_available
	| Restricted of restriction
	| Insufficient_magic_strength

exception Spell_failed of spell_failure
exception Insufficient_funds

let all_spells = ref SpellSet.empty

let level_template = {
	lv_level = 10;
}

let restriction_template = {
	restr_min_level = None;
	restr_wiz_mode = false;
}

let spell_template = {
	spell_name = "(*NO SUCH SPELL*)";
	spell_restriction = restriction_template;
	spell_ms_cost = 0;
	spell_gp_cost = 0;
}

let create_level ~level_number = 
	{ (* level_template with *) lv_level = level_number }

let register_spell ~name ~level =
	let restr = {
		restr_min_level = Some (create_level ~level_number:level);
		restr_wiz_mode = false;
	} in
	let new_spell = {
		spell_name = name;
		spell_restriction = restr;
		spell_ms_cost = 100;
		spell_gp_cost = 100;
	} in
		all_spells := SpellSet.add new_spell !all_spells

let spell_book = [
	("teleport", 10);
	("scry", 2);
]

let init_spell_book () =
	List.iter ( fun (name, level) ->
		register_spell ~name ~level
	) spell_book

let get_spell_by_name ~spell_name =
	let spells = SpellSet.filter (fun elt -> 
		elt.spell_name = spell_name) !all_spells in

		(if SpellSet.cardinal spells = 0
		then raise Not_found);

		assert (SpellSet.cardinal spells = 1); (* this should be pretty invariant *)

		SpellSet.choose spells

let template =
	{ 
		pers_age = 33;
		pers_guild = Guild.Vanilla;
		pers_spells = SpellSet.empty;
		pers_stats = Hashtbl.create 5;
	}

let dummy_max ~level ~guild =
	450000

let score = {
	stat_name = "score";
	stat_prio = 1;
	stat_max = dummy_max;
	stat_default = 0;
}

let quests_done = {
	stat_name = "quests";
	stat_prio = 2;
	stat_max = dummy_max;
	stat_default = 0;
}

let magic_strength = {
	stat_name = "magic strength";
	stat_prio = 3;
	stat_max = dummy_max;
	stat_default = 1000;
}

let gold = {
	stat_name = "gold";
	stat_prio = 4;
	stat_max = dummy_max;
	stat_default = 0;
}
let strength = {
    stat_name = "strength";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let max_strength = {
    stat_name = "maxstrength";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let sin = {
    stat_name = "sin";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let hoopiness = {
    stat_name = "hoopiness";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let fright = {
    stat_name = "fright";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let intelligence = {
    stat_name = "intelligence";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let dexterity = {
    stat_name = "dexterity";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let skill = {
    stat_name = "skill";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let size = {
    stat_name = "size";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let capacity = {
    stat_name = "capacity";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let debt = {
    stat_name = "debt";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let armour = {
    stat_name = "armour";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let lives = {
    stat_name = "lives";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let max_magic = {
    stat_name = "maxmagic";
    stat_prio = 5;
    stat_max = dummy_max;
    stat_default = 0;
}

let stats = [
	score;
	quests_done;
	magic_strength;
	gold;
	strength;
    max_strength;
    sin;
    hoopiness;
    fright;
    intelligence;
    dexterity;
    skill;
    size;
    capacity;
    debt;
    armour;
    lives;
    max_magic;
]

let create () =
	let ss = Hashtbl.create 5 in
		(* wants to be an iter *)
		List.iter (fun stat -> 
			Hashtbl.replace ss stat stat.stat_default) stats;
		{ template with pers_stats = ss }

let stat_set p stat v =
	Hashtbl.replace p.pers_stats stat v

let stat_get p stat =
	Hashtbl.find p.pers_stats stat

let get_stat_name s = s.stat_name
	
let ln2 n =
	let rec ln2 n acc = 
		let n' = n lsr 1 in 
			if n' > 0 
			then ln2 n' (acc+1) 
			else acc
	in
		ln2 n 0

let level_ceiling_by_score ~score =
	ln2 (score / 200)

(* FIXME: should be in some sort of global config *)
let quest_limits = [
	(1, 6); 
	(5, 7); 
	(9, 8); 
	(15, 9); 
	(20, 10); 
	(23, 11); 
	(24, 12); 
	(25, 13);
]

let level_ceiling_by_quests ~quests_done =
	let rec ceiling cur l =
		match l with
			| [] -> cur
			| hd :: tl -> 
				  let quests_required, level = hd in
					  if quests_done < quests_required
					  then cur
					  else ceiling level tl
	in
	let floor = snd (List.hd quest_limits) - 1 in
		ceiling floor quest_limits

let get_level p =
	let score = stat_get p score in
	let quests_done = stat_get p quests_done in
	let ceil_sc = level_ceiling_by_score ~score in
	let ceil_qc = level_ceiling_by_quests ~quests_done in
	let l = min ceil_sc ceil_qc in
		{ lv_level = l }

let change_level p ~old_level =
	print_endline "level!"

let level_compare x y =
	compare x.lv_level y.lv_level

let level_sufficient ~persona ~level =
	let pers_lvl = get_level persona in
		0 <= (level_compare pers_lvl level)

let get_stat persona stat =
	Hashtbl.find persona.pers_stats stat

let stat_incr p stat d =
	(* FIXME level check *)
	let cur = get_stat p stat in
	let newval = cur + d in
	let level = get_level p in
	let guild = p.pers_guild in
	let newval = min newval (stat.stat_max ~level ~guild) in
		Hashtbl.replace p.pers_stats stat newval

let is_necro p =
	p.pers_guild = Guild.Necro

let is_wizard p =
	true

let set_guild p ~guild =
	p.pers_guild <- guild

let get_guild p = p.pers_guild

let string_of_level l =
	string_of_int l.lv_level

let has_spell ~persona ~spell =
	SpellSet.mem spell persona.pers_spells

let spell_allowed ~persona ~wizmode ~spell =
	let req_wiz = spell.spell_restriction.restr_wiz_mode in
	let a =	match req_wiz, wizmode with
		| true, false -> false
		| _, _ -> true
	in 
	let b = match spell.spell_restriction.restr_min_level with
		| None -> true
		| Some level -> level_sufficient ~persona ~level
	in
		a && b

let ms_sufficient ~persona ~spell =
	let ms = get_stat persona magic_strength in
		ms >= spell.spell_ms_cost

(* check: have spell, restriction, ms *)
let use_spell ~persona ~wizmode ~spell = 
	(if not (has_spell ~persona ~spell)
	 then raise (Spell_failed Spell_not_available));

	(if not (spell_allowed ~persona ~wizmode ~spell)
	 then raise (Spell_failed (Restricted spell.spell_restriction)));

	(if not (ms_sufficient ~persona ~spell)
	 then raise (Spell_failed Insufficient_magic_strength));

	stat_incr persona magic_strength (0 - spell.spell_ms_cost)

let use_spell ~persona ~wizmode ~spell_name =
	try
		let spell = get_spell_by_name ~spell_name
		in use_spell ~persona ~wizmode ~spell
	with Not_found -> raise (Spell_failed No_such_spell)

let spend_money ~persona ~gp =
	(if gp > (get_stat persona gold)
	 then raise Insufficient_funds);

	stat_incr persona gold (0 - gp)

let buy_spell ~persona ~spell_name =
	let spell = get_spell_by_name ~spell_name in (* can throw Not_found *)
	let gp = spell.spell_gp_cost in
		(* FIXME *)		   
		(* in right spell shop *)
		assert (not (has_spell ~persona ~spell));

		(* right level *)
		(match spell.spell_restriction.restr_min_level with
			| None -> ()
			| Some level -> assert (level_sufficient ~persona ~level));
			
		(* adjust gold; adjust spells *)
		spend_money ~persona ~gp;
		persona.pers_spells <- SpellSet.add spell persona.pers_spells


let fini () =
	all_spells := SpellSet.empty

let () =
	ignore (
		level_template,
		spell_template,
		stat_set,
		change_level
	)
