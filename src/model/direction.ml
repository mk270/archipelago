(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type direction = {
	dir_name : string;
	dir_whence : string;
	dir_whither : string;
	dir_code_number : int;
}

let compass = ref []

let direction_of_int n =
	let matches = List.filter (fun i -> i.dir_code_number = n) !compass in
		match matches with
			| [] -> raise Not_found
			| hd :: _ -> hd

let create_direction ~dir_name ~whence ~whither ~dir_number =
	let dir = {
		dir_name = dir_name;
		dir_whence = whence;
		dir_whither = whither;
		dir_code_number = dir_number;
	} in
		try ignore(direction_of_int dir_number);
			failwith "Duplicate direction ID"
		with Not_found -> 
			compass := dir :: !compass;
			dir

let string_of_direction dir = dir.dir_name
let whence dir = dir.dir_whence
let whither dir = dir.dir_whither
let int_of_direction dir = dir.dir_code_number

let match_direction s =
	let names = List.map (fun d -> (d, string_of_direction d)) !compass in
	let matches = List.filter (
		fun name -> Utils.initial_match s (snd name)
	) names in
		match matches with
			| [] -> raise Not_found
			| hd :: _ -> fst hd

let get_compass () =
	!compass

let clear () = compass := []
