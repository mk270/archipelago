(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Weather

let bound a minimum maximum = min (max a minimum) maximum
let bound_09 a = bound a 0 9 

let fix_weather w =
	if w.w_rain <= w.w_clouds
	then w
	else match (Random.bool ()) with
		| true ->  { w with w_rain = w.w_clouds }
		| false -> { w with w_clouds = w.w_rain }

let weather () =
	let change_type = Random.int 4 in
	let delta = 2 * (Random.int 2) - 1 in
	let new_weather = match change_type with
		| 0 -> { !global_weather with 
					 w_wind = bound_09 (!global_weather.w_wind + delta) }
		| 1 -> { !global_weather with 
					 w_rain = bound_09 (!global_weather.w_rain + delta) }
		| 2 -> { !global_weather with 
					 w_clouds = bound_09 (!global_weather.w_clouds + delta) }
		| 3 -> { !global_weather with 
					 w_temperature = bound_09 (!global_weather.w_temperature + delta) }
		| _ -> assert false
	in
		global_weather := fix_weather new_weather;
		!global_weather

let atmos_daemon_init ~get_state ~construct_event ~frequency = 
	let state = ref (get_state ()) in
	let tick_fn = fun ~self ->
		let new_state = get_state () in
			if !state <> new_state
			then ( state := new_state;
				   Model.post_event (construct_event new_state) )
	in
	let dmn = Daemon.create ~frequency ~tick_fn in
		Daemon.start dmn

let sun_daemon_init () =
	let get_state = fun () -> sun () in
	let construct_event = fun new_state -> Model.NewSun new_state in
	let frequency = sun_freq in
		atmos_daemon_init ~get_state ~construct_event ~frequency
	
let moon_daemon_init () = 
	let get_state = fun () -> moon () in
	let construct_event = fun new_state -> Model.NewMoonState new_state in
	let frequency = moon_freq in
		atmos_daemon_init ~get_state ~construct_event ~frequency

let weather_daemon_init () = 
	let get_state = fun () -> weather () in
	let construct_event = fun new_state -> Model.NewWeather new_state in
	let frequency = weather_freq in
		atmos_daemon_init ~get_state ~construct_event ~frequency

let init () =
	sun_daemon_init ();
	moon_daemon_init ();
	weather_daemon_init ()

(* notice new weather:

 get location-adjusted new weather (a daemon updates the weather separately)

 player has a cache of: 
  whether it's day or night
  what sort of precipitation
  what the weather he last experienced was

 notify if day/night has changed

 give up if nothing interesting has happened
 (the routine can be run iteratively, and will reel off
  the interesting stuff until it gives up)

 check how much the change in the specified weather attribute has been

 do the effect, and if it's a rising effect, add some terrain-specific info


*)
