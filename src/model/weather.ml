(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


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

type weather = { 
	w_wind : int;
	w_rain : int;
	w_clouds : int;
	w_temperature : int;
}

let default_weather = {
	w_wind = 5;
	w_rain = 5;
	w_clouds = 5;
	w_temperature = 5;
}

let global_weather = ref default_weather

let new_sun_msg = function
	| Day -> "It is now broad daylight"
	| Dusk -> "It is getting darker"
	| Night -> "It is now dark"
	| Dawn -> "It is getting lighter"

let string_of_sun = function
	| Day -> "daytime"
	| Dusk -> "dusk"
	| Night -> "nighttime"
	| Dawn -> "dawn"

let string_of_rain = function
	| Rain -> "rain"
	| Sleet -> "sleet"
	| Snow -> "snow"
	| Hail -> "hail"
	| ElectricStorm -> "electric storm"

let sun_msg sun_state =
	"It is " ^ (string_of_sun sun_state) ^ "."

let string_of_moon = function
	| NewMoon -> "new"
	| Waxing -> "waxing"
	| Full -> "full"
	| Waning -> "waning"

let moon_msg moon_state =
	"The moon is " ^ (string_of_moon moon_state) ^ "."


let sun_period = 2400
let sun_freq = 47.0
let moon_period = 10230
let moon_freq = 257.0

let phase_segments = 8
let moon_phases = 4

let weather_freq = 61.23

let pos_mod x y =
	if (x mod y) >= 0
	then x mod y
	else (x mod y) + y

let sun now =
	let phase = (pos_mod now sun_period) / (sun_period / phase_segments) in
		match phase with
			| 0	| 1	| 2 -> Day
			| 3         -> Dusk
			| 4	| 5	| 6 -> Night
			| 7         -> Dawn
			| _ -> assert false (* value must be < phase_segments *)

let sun () =
	let now = int_of_float (Unix.gettimeofday ()) in
		sun now

let moon () =
	let now = int_of_float (Unix.gettimeofday ()) in
	let phase = (pos_mod now moon_period) / (moon_period / moon_phases) in
		match phase with
			| 0 -> NewMoon
			| 1 -> Waxing
			| 2 -> Full
			| 3 -> Waning
			| _ -> assert false (* value must be < moon_phases *)
				(* FIXME: apparently this assertion can fail! *)
