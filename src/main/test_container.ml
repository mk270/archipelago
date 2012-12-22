(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open OUnit

let create_tree () =
	let t0 = Container.create "The Queen"
	and t1 = Container.create "Prince of Wales"
	and t2 = Container.create "Duke of Cambridge"
	and t3 = Container.create "Prince Harry" in
		Container.insert_into t0 t1;
		Container.insert_into t1 t2;
		Container.insert_into t1 t3;
		t0
(*
let test_raise () =
	assert_raises Not_found (fun () ->
		String.index "hello" 'a')
*)

let test_basic () =
	let t0 = create_tree () in
	let children = Container.children t0 in
	let names = List.map Container.contained children in
		match names with
			| [] -> assert_failure "no children in root node"
			| [s] -> assert_equal s "Prince of Wales"
			| x :: xs -> assert_failure "too many children in root node"

let suite =
	"container" >::: [
		"test_basic" >:: test_basic;
	]

let main () =
	let results = run_test_tt_main suite in
		ignore results
