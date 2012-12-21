(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type weather = { 
	w_wind : int;
	w_rain : int;
	w_clouds : int;
	w_temperature : int;
}

type time_of_day =
	| Dawn
	| Day
	| Dusk
	| Night

type phase_of_moon =
	| NewMoon
	| Waxing
	| Full
	| Waning

type precipitation =
	| Rain
	| Sleet
	| Snow
	| Hail
	| ElectricStorm

val default_weather : weather

val global_weather : weather ref

val sun_freq : float
val moon_freq : float
val weather_freq : float

val sun : unit -> time_of_day
val moon : unit -> phase_of_moon

val string_of_rain : precipitation -> string
val new_sun_msg : time_of_day -> string
val sun_msg : time_of_day -> string
val moon_msg : phase_of_moon -> string
