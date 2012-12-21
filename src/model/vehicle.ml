(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type vehicle =
	| SeaBoat
	| RiverBoat
	| Car
	| Submarine
	| Canoe
	| Railcar
	| Spacepod
	| Shuttle
	| Microlight
	| Plane

exception Invalid_vehicle

let vehicle_of_string = function
    | "0" -> SeaBoat
    | "1" -> RiverBoat
    | "2" -> Car
    | "3" -> Submarine
    | "4" -> Canoe
    | "5" -> Railcar
    | "6" -> Spacepod
    | "7" -> Shuttle
    | "8" -> Microlight
    | "9" -> Plane
	| _ -> raise Invalid_vehicle
