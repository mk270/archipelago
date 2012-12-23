(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open OUnit

let data = 
	[ 
		(100.0, "hundred");
		(50.0, "fifty");
		(300.0, "three hundred");
		(1.5, "one and a half");
		(12.0, "twelve");
		(1000.0, "grand");
		(0.0, "zilch");
	]

let data_sorted = 
	[ 
		(0.0, "zilch");
		(1.5, "one and a half");
		(12.0, "twelve");
		(50.0, "fifty");
		(100.0, "hundred");
		(300.0, "three hundred");
		(1000.0, "grand");
	]

let rec populate l acc =
	match l with
		| [] -> acc
		| hd :: tl -> populate tl (Prioqueue.insert acc (fst hd) (snd hd) )

let display_prio_elt prio elt =
	Printf.printf "Prio: %3.3f; Elt: [%s]\n\n" prio elt

let exhaust_prioq pq =
	Prioqueue.iter display_prio_elt pq

let exhaust_prioq_to_limit ~comparand pq =
	Prioqueue.iter ~comparand display_prio_elt pq

let test_exhaust_partially () =
	let pq = populate data Prioqueue.empty in
	let pq = exhaust_prioq_to_limit ~comparand:123.45 pq in
	let prio, observed, pq = Prioqueue.extract pq in
		assert_equal prio 300.0

let test_triv2 () =
	let pq = populate data Prioqueue.empty in
	let prio, observed, pq = Prioqueue.extract pq in
	let expected = "zilch" in
		assert_equal observed expected

let suite = 
	"prioq_suite" >::: [
		"test2" >:: test_triv2;
		"test3" >:: test_exhaust_partially;
	]
	
let run () =
	run_test_tt_main suite

let main () =
	ignore(run ())
