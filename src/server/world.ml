(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Model
open Direction
open Name
open Sqlite3
open Aperture

let db_file = "../db/arch.db"

exception World_misconfigured

type aperture_info = {
	apinf_state : string;
	apinf_trapped : string;
	apinf_accessor : int option;
}

type special =
	| StartRoom
	| ThePit
	| UnderThePit

let special_objects = Hashtbl.create 10

let registry = Hashtbl.create 2500
let parents  = Hashtbl.create 1000
let descriptions = Hashtbl.create 1000
let links : (int * int, int * mudobject option) Hashtbl.t = Hashtbl.create 2500
let portals = Hashtbl.create 500
let doors = Hashtbl.create 50
let apertures = Hashtbl.create 100
let door_objects = Hashtbl.create 50
let flags = Hashtbl.create 300
let stats = Hashtbl.create 1500
let object_attributes = Hashtbl.create 2000
let link_req_items = Hashtbl.create 300

let get_start_room () =
	try Hashtbl.find special_objects StartRoom
	with Not_found -> failwith "World not initialised properly!"

let get_a_room id =
	try Hashtbl.find special_objects id
	with Not_found -> raise World_misconfigured

let number_of_int = function
	| 1 -> Singular
	| 2 -> Plural
	| _ -> failwith "unknown number (as it were)"

let s_of_so = function
	| Some s -> s
	| None -> failwith "compulsory option not taken"  

let clear_hash () =
	Hashtbl.clear registry;
	Hashtbl.clear parents;
	Hashtbl.clear descriptions;
	Hashtbl.clear links;
	Hashtbl.clear portals;
	Hashtbl.clear doors;
	Hashtbl.clear apertures;
	Hashtbl.clear door_objects;
	Hashtbl.clear flags;
	Hashtbl.clear stats;
	Hashtbl.clear object_attributes;
	Hashtbl.clear link_req_items

let parent_objects () =
	Hashtbl.iter (fun k v -> 
					  Tree.insert_into 
						  ~recipient:(Hashtbl.find registry v) 
						  (Hashtbl.find registry k) ) parents

let special_of_name = function
	| "Limbo" -> StartRoom
	| "The Pit" -> ThePit
	| "Under the Pit" -> UnderThePit
	| _ -> raise Not_found

let register_location ~id ~name ~adam ~num =
	let desc = Hashtbl.find descriptions id in
	let loc_code = Hashtbl.find object_attributes (id, "loc_code") in
	let terrain = Hashtbl.find object_attributes (id, "terrain") in
	let o = Create.create_room (name, adam, num) ~desc:desc ~loc_code ~terrain in
		Hashtbl.replace registry id o;
		(try let special = special_of_name name in
			 Hashtbl.replace special_objects special o
		 with Not_found -> ())

let register_item ~id ~name ~adam ~num =
	let desc = 
		try Hashtbl.find descriptions id 
		with Not_found -> "This object has no description."
	in
	let flags = Hashtbl.find_all flags id in
	let stats = Hashtbl.find_all stats id in
	let stats = 
		try let v = Hashtbl.find object_attributes (id, "vehicle") in
			("vehicle", v) :: stats
		with Not_found -> stats
	in
	let o = Create.create_item (name, adam, num) ~desc ~flags ~stats in
		Hashtbl.replace registry id o

let register_monster ~id ~name ~adam ~num =
	let sex = Hashtbl.find object_attributes (id, "gender") in
	let sex = Sex.sex_of_string sex in
	let o = Create.create_monster (name, adam, num) ~sex in
		Hashtbl.replace registry id o

let register_door ~id ~name ~adam ~num =
	(*print_int id; print_string name; 
	  print_endline " door";*)
	Hashtbl.replace doors id (name, adam, num)

let register_object id name adam num otype parent =
	let id = int_of_string (s_of_so id) in
	let name = s_of_so name in
	let adam = adam_of_string (s_of_so adam) in
	let num = number_of_int (int_of_string (s_of_so num)) in
	let otype = s_of_so otype in
	let parent = int_of_string (s_of_so parent) in
		if parent != 0 
		then Hashtbl.replace parents id parent;
		match otype with
			| "Location" -> register_location ~id ~name ~adam ~num
			| "Item" -> register_item ~id ~name ~adam ~num
			| "Monster" -> register_monster ~id ~name ~adam ~num
			| "Door" -> register_door ~id ~name ~adam ~num
			| _ -> failwith "unrecognised object type"

let object_row row header = 
	(
		match row with
			| [|id;name;adam;num;otype;parent|] -> 
				  register_object id name adam num otype parent
			| _ -> failwith "Database schema error (or something)"
	); 
	()

let objattr_row row header =
	match row with
		| [| id ; name ; value |] ->
			  let id = int_of_string (s_of_so id) in
			  let name = s_of_so name in
			  let value = s_of_so value in
				  Hashtbl.replace object_attributes (id, name) value 
		| _ -> failwith "Database schema error (or something)"

let objlink_row row header =
	match row with
		| [| src ; dst ; dir |] -> 
			  let src = int_of_string (s_of_so src) in
			  let dst = int_of_string (s_of_so dst) in
			  let dir = int_of_string (s_of_so dir) in
				  (*  add_link 
					  (Hashtbl.find registry src) 
					  (Hashtbl.find registry dst) dir None *)
				  Hashtbl.replace links (src, dir) (dst, None)
		| _ -> assert false

let linkobj_row row header =
	match row with
		| [| src ; dst ; obj ; dir |] ->
			  let src = int_of_string (s_of_so src) in
			  let dst = int_of_string (s_of_so dst) in
			  let dir = direction_of_int (int_of_string (s_of_so dir)) in
			  let obj = int_of_string (s_of_so obj) in
				  Hashtbl.replace portals (src, dst, dir) obj
		| _ -> assert false

let linkreqobj_row row header =
	match row with
		| [| src ; dst ; obj |] ->
			  let src = int_of_string (s_of_so src) in
			  let dst = int_of_string (s_of_so dst) in
			  let obj = int_of_string (s_of_so obj) in
			  let obj' = Hashtbl.find registry obj in
				  Hashtbl.replace link_req_items (src, dst) obj'
		| _ -> assert false

let portal_for_link src dst dir =
	try
		let p = Hashtbl.find portals (src, dst, dir) in
		let door = 
			try
				Hashtbl.find doors p
			with Not_found -> 
				assert false
		in
		let pp = 
			try Hashtbl.find door_objects door
			with Not_found ->
				let obj = (Create.create_portal door Open) in
					Hashtbl.replace door_objects door obj;
					Hashtbl.replace registry p obj;
					obj				
		in
			Some pp
	with Not_found -> None

let req_item_for_link src dst = 
	try
		Some (Hashtbl.find link_req_items (src, dst))
	with Not_found -> None

let link_links () =
	Hashtbl.iter (fun k v ->
					  let src, dir = k in
					  let dst, _ = v in
					  let dir = direction_of_int dir in
					  let p = portal_for_link src dst dir in
					  let req_item = req_item_for_link src dst in
						  Link.add_link (Hashtbl.find registry src)
							  (Hashtbl.find registry dst) dir p req_item
				 ) links

let description_row row header =
	match row with
		| [| id ; desc |] ->
			  Hashtbl.replace descriptions 
				  (int_of_string (s_of_so id))
				  (s_of_so desc)
		| _ -> assert false

let objflag_row row header =
	match row with
		| [| id ; flag |] ->
			  Hashtbl.add flags (int_of_string (s_of_so id))
				  (s_of_so flag)
		| _ -> assert false

let objstat_row row header =
	match row with
		| [| id ; stat_id ; stat_value |] ->
			  Hashtbl.add stats (int_of_string (s_of_so id))
				  ( (s_of_so stat_id), (s_of_so stat_value) )
		| _ -> assert false

let aperture_row row header =
	match row with
		| [| id ; state ; trapped ; accessor |] ->
			  let id = int_of_string (s_of_so id) in
			  let state = s_of_so state in
			  let trapped = s_of_so trapped in
			  let accessor = match accessor with
				  | Some s -> Some (int_of_string s)
				  | None -> None
			  in
				  Hashtbl.replace apertures id
					  { apinf_state=state;
						apinf_trapped=trapped;
						apinf_accessor=accessor;
					  }
					  (*	  (int_of_string (s_of_so id))
							  ((s_of_so state), 
							  (s_of_so trapped), 
							  (int_of_string (s_of_so accessor))) *)
		| _ -> assert false

let link_apertures () =
	Hashtbl.iter ( fun k v -> 
					   let state' = Model.Link.apstate_of_string v.apinf_state in
					   let state = (
						   match state', v.apinf_trapped with
							   | Open, "f" -> Open
							   | Closed, "f" -> Closed
							   | Closed, "t" -> Trapped
							   | Locked, "f" -> Locked
							   | _ -> assert false
					   ) in
					   let accessor = ( 
						   match v.apinf_accessor with
							   | None -> None
							   | Some a -> Some (Hashtbl.find registry a)
					   ) in
					   let ap = Model.Create.create_aperture state accessor in
					   let obj = Hashtbl.find registry k in
						   Model.Props.set_aperture obj (Some ap)
	) apertures

let setup_directions () =
	(* this should ultimately move to DB *)

	let compass = [
		("north", None, None, 0);
		("east", None, None, 1);
		("south", None, None, 2);
		("west", None, None, 3);
		("northeast", None, None, 4);
		("southeast", None, None, 5);
		("southwest", None, None, 6);
		("northwest", None, None, 7);
		("up", Some "above", Some "upwards", 8);
		("down", Some "below", Some "downwards", 9);
		("in", Some "inside", Some "inside", 10);
		("out", Some "outside", Some "outside", 11);
		("pit", Some "the direction of the pit", Some "towards the pit", 12);
		("fore", Some "foreward", Some "fore", 13);
		("aft", Some "aftward", Some "aft", 14);
		("port", Some "the port side", Some "to the port side", 15);
		("starboard", Some "the starboard side", Some "to the starboard side", 16);
	] in
	let f (dir_name, whence, whither, dir_number) =
		let whence  = match whence with
			| None -> "the " ^ dir_name
			| Some s -> s
		in
		let whither = match whither with
			| None -> dir_name
			| Some s -> s
		in 
		ignore( Direction.create_direction 
			~dir_name ~whence ~whither ~dir_number )
	in
		List.iter f (List.rev compass)

let table_info = [
	( "SELECT id, state, trapped, accessor FROM aperture", aperture_row );
	( "SELECT id, desc FROM description", description_row );
	( "SELECT id, flag FROM objflag", objflag_row );
	( "SELECT id, statidx, statval FROM objstat", objstat_row );
	( "SELECT id, attrname, attrvalue FROM objattr", objattr_row );
	( "SELECT id, name, adam, num, otype, parent FROM object", object_row );
	( "SELECT src, dst, dir FROM objlink", objlink_row );
	( "SELECT src, dst, obj, dir FROM linkobj", linkobj_row );
	( "SELECT src, dst, obj FROM linkreqobj", linkreqobj_row );
]

let load_table ~db ~table =
	let sql, cb = table in
		ignore(exec db ~cb sql)

let load_tables ~db =
	List.iter (fun table -> load_table ~db ~table) table_info

let load_world_from_db () =
	let db = db_open db_file in
		load_tables ~db;
		ignore(db_close db);
		parent_objects ();
		link_links ();
		link_apertures ();
		clear_hash ();
		()

let init () = 
	setup_directions ();
    load_world_from_db ();
    ()

let flush_special_objects () = 
	Hashtbl.clear special_objects

let clear_directions () = 
	Direction.clear ()

let destroy_world () = 
	Model.destroy_all ()

let fini () =
	flush_special_objects ();
	clear_directions ();
	destroy_world ()
