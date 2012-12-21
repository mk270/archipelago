(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type adam = Definite | Indefinite | Vocalic | Mass | NoAdam

type number = Singular | Plural

type name = string * adam * number

val vague : name -> string
val unvague : name -> string

val here_msg : name -> string

val obj_name : name -> string

val adam_of_string : string -> adam
