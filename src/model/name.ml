(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Printf

type adam = Definite | Indefinite | Vocalic | Mass | NoAdam

type number = Singular | Plural

type name = string * adam * number

let article_v = function
  | Definite -> "the "
  | Indefinite -> "a "
  | Vocalic -> "an "
  | NoAdam -> ""
  | Mass -> "some "

let article_uv = function
  | Definite
  | Indefinite
  | Vocalic
  | Mass -> "the "
  | NoAdam -> ""

let vague n = 
  let s, a, p = n in
  let art = article_v a in
    art ^ s

let unvague n = 
  let s, a, p = n in
  let art = article_uv a in
    art ^ s

let verb_to_be = function
  | Singular -> "is"
  | Plural -> "are"

let here_msg n =
  let s, a, p = n in
    match (a, p) with
      | (NoAdam, pp) 
      | (Definite, pp) -> sprintf "%s %s here." (vague n) (verb_to_be pp) 
      | (Indefinite, pp)
      | (Mass, pp)
      | (Vocalic, pp) -> sprintf "There %s %s here." (verb_to_be pp) (vague n)

let obj_name n = 
  let s, a, p = n in
    s

let adam_of_string = function
  | "I" -> Indefinite
  | "N" -> NoAdam
  | "V" -> Vocalic
  | "D" -> Definite
  | "P" -> Mass
  | _ -> failwith "unknown adam"
