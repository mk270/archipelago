(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Unix

let really_read fd =
  let max_read = 4096 in
  let rec read_all acc =
    let buf = String.create max_read in
      try
        let pos = 0 in
        let len_read = read fd buf pos max_read in
	  if len_read = 0 then
            acc
          else
            let tmp = String.sub buf 0 len_read in
                read_all (acc ^ tmp)
      with 
		  | Unix_error (EAGAIN, _, _)
		  | Unix_error (ECONNRESET, _, _) ->
				acc (* FIXME - need to catch all errors *)
  in
    read_all ""

let really_write fd buf =
  let max_write = 4096 in
  let rec write_all offset =
    let size = min max_write (String.length buf - offset) in
      if size = 0
      then
	offset
      else
	try 
	  let len_written = write fd buf offset size in
	    write_all (offset + len_written) 
	with
	    Unix_error (EAGAIN, _, _) -> offset
  in
    write_all 0
