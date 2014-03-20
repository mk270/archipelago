(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)



let remove needle haystack = List.filter (fun x -> x != needle) haystack;;

let rec comma_sep l =
  match l with
    | [] -> ""
    | [one] -> one
    | [one; two] -> one ^ " and " ^ two
    | hd :: tl   -> hd  ^ ", "    ^ (comma_sep tl)

let initial_match a b =
	let al = String.length a in
	let bl = String.length b in
	let a = String.lowercase a in
	let b = String.lowercase b in
	let common = min al bl in
		(al <= bl) && (String.sub a 0 common) = (String.sub b 0 common)

let initial_match a b =
	let words = Str.split (Str.regexp " +") b in
	let f word = initial_match a word in
		List.fold_left (fun acc word -> acc || (f word)) false words

let random_list_member l = List.nth l (Random.int (List.length l))

let guard_exception e =
	let msg = "An error occurred: " ^ (Printexc.to_string e) in
	let bt = Printexc.get_backtrace () in
		print_endline bt;
		print_endline msg;
		flush_all ()

let tally l =
    let f a elt =
		let new_elt = 
			if List.mem_assq elt a
			then (elt, (1 + List.assq elt a))
       		 else (elt, 1)
		in 
		   new_elt :: List.remove_assq elt a
    in
        List.fold_left f [] l


