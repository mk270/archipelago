(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type t = { lc_name : string }

exception Invalid_loccode of string

let create s =
	try 
		assert (3 = String.length s);
		let superblock = s.[0] in
		let block = s.[1] in
		let page = s.[2] in
			assert (superblock >= 'A');
			assert (superblock <= 'E');
			assert (page >= 'A');
			assert (page <= 'P');
			assert (((block >= 'A') && (block <= 'Z')) ||
					((block >= '0') && (block <= '9')));
			{ lc_name = s }
	with Assert_failure _ ->
		raise (Invalid_loccode s)

let string_of_loc_code lc = lc.lc_name
