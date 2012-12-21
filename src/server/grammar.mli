(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Model

type tense_aspect =
	| Present
	| Perfect

type person = Third | Second | First 

exception Missing_grammar_argument
exception Malformed_verb_spec

val render : ?actor : mudobject -> ?patient : mudobject -> 
	?person : person -> ?tense : tense_aspect -> string -> string
