(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


let hack_passwd_db = Hashtbl.create 5

(* FIXME *)
let init_hack_passwd_db () =
  (
      Hashtbl.replace hack_passwd_db "Wizard" "wizard";
      flush_all ()
  )

let sensible_name n =
	let regexp = Str.regexp "^[A-Za-z][a-zA-Z0-9]*$" in
	let len = String.length n in
		(len >= 3) && (len < 16) && (Str.string_match regexp n 0)

let normalise_name n =
  String.capitalize n

let sensible_password p = true

let player_name_known name =
	Hashtbl.mem hack_passwd_db name

let password_matches name check = 
  try
    let p = Hashtbl.find hack_passwd_db name in
      p = check
  with Not_found ->
    (* WTF?! *)
    false

let unique_len = 3

let name_too_similar name =
	let name = String.lowercase name in
	let cmp x y = (String.sub x 0 unique_len) = (String.sub y 0 unique_len) in
	let f k v a = 
		let k = String.lowercase k in
			a || (cmp k name)
	in
		Hashtbl.fold f hack_passwd_db false

let name_too_similar ~name =
	try name_too_similar name
	with Invalid_argument _ -> false

let init () =
	init_hack_passwd_db ()

let fini () =
	Hashtbl.clear hack_passwd_db
