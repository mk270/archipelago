(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Terrain
open Weather

let dawn_effect = function
	| Hardground 
	| Softground 
	| Sand 
	| Cyclone 
	| Marsh -> " as the sun creeps over the eastern horizon"
	| Woods -> ", and the trees cast long shadows in the morning sun"
	| Jungle -> ", and the rays of the morning sun illuminate the jungle"
	| Water -> ", and the water sparkles in the morning sun"
	| Arctic -> ", and the snowscape becomes a dazzling white in the morning sun"

let dusk_effect = function
	| Hardground
	| Arctic
	| Sand
	| Cyclone
	| Softground
	| Marsh -> " as the sun sinks over the western horizon"
	| Woods -> ", and the trees cast long shadows in the evening sun"
	| Jungle -> ", and the rays of the setting sun dance through the foliage"
	| Water -> ", and the setting sun casts a thick red streak on the water"

let new_wind ~player ~indoors ~current_weather ~new_weather ~climate =
	let cmp = new_weather.w_wind > current_weather.w_wind in
	let msg =
		match cmp, indoors with
			| true,  false -> "The wind rises"
			| true,  true  -> "The wind rises outside"
			| false, false -> "The wind dies down"
			| false, true  -> "The wind dies down outside"
	in
	let msg2 =
		match cmp, indoors with
			| true, false ->
				 ( match climate with
					   | Woods -> " and whistles through the trees"
					   | Jungle ->  " and howls through the jungle"
					   | Water -> " and whips up spray into your face"
					   | Arctic -> " and drives the snow across the land"
					   | Sand -> " and blows the sand around in whirlwinds"
					   | Cyclone -> " and almost knocks you to the ground."
					   | _ -> ""
				 )
			| _ -> ""
	in
	let w = { current_weather with w_wind = new_weather.w_wind }
	in
		Game.emitl player (msg ^ msg2 ^ ".");
		Model.Props.set_local_weather player w
			(* FIXME: blowout *)

(*
  if cur and new are > 4, check whether the type of rain has changed
  if so, announce the change
  otherwise, announce heavier/lighter

  if cur < 4 but new > 4, announce starts to fall / breakout of storm
  if cur > 4 but new < 4, announce cessation

  in latter two cases, we don't do the terrain-specific message

*)

(* FIXME: blowout *)

let prep_effect rain_type climate =
	match rain_type, climate with
		| Rain, Hardground -> " and pounds against the hard ground"
		| Rain, Softground -> " and forms puddles on the soft ground"
		| Rain, Marsh -> " and sinks into the marsh"
		| Rain, Woods -> " and drips off the leaves of the trees"
		| Rain, Jungle -> " and drips off the creepers"
		| Rain, Water -> " and makes patterns on the water like rain(6)"
		| Rain, Arctic -> " and starts to melt the snow"
		| Snow, Hardground -> " and settles on the hard ground"
		| Snow, Softground -> " and settles slowly"
		| Snow, Marsh -> " and melts in the marsh"
		| Snow, Woods -> " and settles in the undergrowth"
		| Snow, Water -> " and melts in the water"
		| Snow, Sand -> " and covers the sand in a white blanket"
		| Hail, Hardground -> " and bounces up off the hard ground"
		| Hail, Marsh -> " and sinks into the marsh"
		| Hail, Water -> " and melts slowly in the water"
		| Hail, Sand -> " and mingles with the sand"
		| ElectricStorm, Hardground 
		| ElectricStorm, Softground 
		| ElectricStorm, Water
		| ElectricStorm, Cyclone
		| ElectricStorm, Marsh ->  " and you are almost struck by a bolt of lightning"
		| ElectricStorm, Woods
		| ElectricStorm, Jungle -> " and a nearby tree is struck by lightning" 
		| ElectricStorm, Arctic -> " and a large area of snow is sublimed by a bolt of lightning"
		| ElectricStorm, Sand -> " and some of the sand is fused to glass by a bolt of lightning"
		| _ -> raise Not_found

let do_precipiation_msg rain climate light =
	match rain, climate with
		| Rain, c
		| Hail, c
		| Sleet, c when (light || c = Hardground) -> true
		| Snow, _ when light -> true
		| ElectricStorm, _ -> true
		| _ -> false

let precipitation_msg rain climate light =
	if do_precipiation_msg rain climate light
	then try prep_effect rain climate with Not_found -> ""
	else ""

let rain_type weather =
	match weather.w_temperature, weather.w_rain with
		| 0, _
		| 1, _ -> Snow
		| 2, _ -> Sleet
		| t, r when (t < 5 && r > 7) -> Hail
		| _, r when (r > 8) -> ElectricStorm
		| _ -> Rain

let rain_is_heavy weather =
	weather.w_rain > 4

let new_rain ~player ~indoors ~current_weather ~new_weather ~climate =
	let new_rain = new_weather.w_rain in
	let cur_rain = current_weather.w_rain in
	let new_rain_type = rain_type new_weather in
	let cur_rain_type = rain_type current_weather in
	let new_rain_name = string_of_rain new_rain_type in
	let cur_rain_name = string_of_rain cur_rain_type in
	let new_heavy = rain_is_heavy new_weather in
	let cur_heavy = rain_is_heavy current_weather in

	let follow_on, msg =
		match cur_rain, new_rain, cur_heavy, new_heavy, cur_rain_type, new_rain_type with
			| _, _,   true, true, crt, nrt when (crt <> nrt) -> (true, "the " ^ cur_rain_name ^ " turns to " ^ new_rain_name)
			| cr, nr, true, true, _,   _   when (nr > cr) ->    (true, "the " ^ cur_rain_name ^ " gets heaver")
			| cr, nr, true, true, _,   _   when (nr < cr) ->	(true, "the " ^ cur_rain_name ^ " gets lighter")
			| _, _, false, true, _, ElectricStorm -> (false, "a violent electric storm appears, seemingly out of nowhere") 
			| _, _, false, true, _, Snow
			| _, _, false, true, _, Rain -> (false, "it starts to " ^ new_rain_name)
			| _, _, false, true, _, _    -> (false, new_rain_name ^ " starts to fall")
			| _, _, true, false, _,  _   -> (false, "the " ^ cur_rain_name ^ " stops")
			| _, _, true, true, _, _
			| _, _, false, false, _, _ -> 
				  Printf.printf "rain error: new(%d) cur(%d)\n" new_rain cur_rain; flush_all ();
				  (false, "")
	in
		
	let msg' = String.capitalize (
		match indoors with
			| true -> "outside, " ^ msg
			| false -> msg
	)
	in
	let light = Model.Light.player_can_see player in
	let msg2 = precipitation_msg new_rain_type climate light in
	let w = { current_weather with w_rain = new_weather.w_rain }
	in
		ignore(follow_on);
		Model.Props.set_local_weather player w;
		(if "" <> msg
		 then Game.emitl player (msg' ^ msg2 ^ "."))

let handle_clouds ~player ~sudden ~body cloudy_current cloudy_new =
	let emit = Game.emitl player in
	let spr = Printf.sprintf in
		match cloudy_current, cloudy_new, sudden with
			| true, false, true  ->
				  emit (spr "Suddenly, the clouds disperse, and the %s appears." body)
			| true, false, false -> 
				  emit (spr "The %s comes out from behind the clouds." body)
			| false, true, true  -> 
				  emit (spr "Suddenly, the %s disappears behind some thick clouds." body)
			| false, true, false -> 
				  emit (spr "The skies cloud over, obscuring the %s." body)
			| _ -> ()

let new_clouds ~player ~indoors ~current_weather ~new_weather =
	let rate = abs (new_weather.w_clouds - current_weather.w_clouds) in
	let sudden = rate > 2 in
	let cloudy w = w.w_clouds >= 5 in
	let body = match (sun (), moon ()) with
		| Day, _       -> Some "sun"
		| _,   NewMoon -> None
		| _,   Full    -> Some "full moon"
		| _,   _       -> Some "moon"
	in
	let w = { current_weather with w_clouds = new_weather.w_clouds }
	in
		Model.Props.set_local_weather player w;
		match body with
			| None -> ()
			| Some body -> handle_clouds ~player ~sudden ~body (cloudy current_weather) (cloudy new_weather)

let new_temperature ~player ~indoors ~current_weather ~new_weather =
	let rising = new_weather.w_temperature > current_weather.w_temperature in
	let rate = new_weather.w_temperature - current_weather.w_temperature in
	let sudden = rate > 2 in
	let msg =
		match rising, sudden, indoors with
			| true,  true, true -> "Suddenly, the weather gets hotter"
			| true,  _, _       -> "It gets warmer"
			| false, true, true -> "Suddenly, the weather gets cooler"
			| false, _, _       -> "It gets cooler"
	in
	let msg2 =
		if not (rain_is_heavy current_weather)
		then ""
		else 
			if (rain_type current_weather) = (rain_type new_weather)
			then ""
			else 
				let cr_name = string_of_rain (rain_type current_weather) in
				let nr_name = string_of_rain (rain_type new_weather) in
				let tmpl = "and the " ^ cr_name ^ " turns to " ^ nr_name in
					match indoors with
						| true -> " outside, " ^ tmpl
						| false -> ", " ^ tmpl
	in
	let w = { current_weather with w_temperature = new_weather.w_temperature }
	in
		Game.emitl player (msg ^ msg2 ^ ".");
		Model.Props.set_local_weather player w

let new_time_of_day ~player ~new_weather ~climate ~current_tod ~new_tod =
	Model.Props.set_time_of_day player new_tod;
	if new_weather.w_clouds < 5
	then match new_tod with
		| Dawn -> Game.emitl player ((new_sun_msg new_tod) ^ (dawn_effect climate) ^ ".")
		| Dusk -> Game.emitl player ((new_sun_msg new_tod) ^ (dusk_effect climate) ^ ".")
		| _ -> ()

let adjust_weather ~player ~climate new_weather =
	let weather = 
		match climate with
			| Jungle -> {	new_weather with
								w_clouds = new_weather.w_clouds + 5;
								w_temperature = new_weather.w_temperature + 5;
						}
			| Water -> { new_weather with
							 w_wind = new_weather.w_wind + 2;
					   }						
			| Arctic -> { new_weather with
							  w_temperature = new_weather.w_temperature - 8;
						}
			| Cyclone -> {
				  w_wind = new_weather.w_wind + 8;
				  w_rain = new_weather.w_rain + 8;
				  w_clouds = new_weather.w_clouds + 8;
				  w_temperature = 7;
			  }
			| _ -> new_weather
	in
	let weather =
		match (sun ()) with
			| Day -> { weather with w_temperature = weather.w_temperature + 1; }
			| Night -> {weather with w_temperature = weather.w_temperature - 1;}
			| _ -> weather
	in
		{ w_wind = max 0 weather.w_wind;
		  w_rain = max 0 weather.w_rain;
		  w_clouds = max 0 weather.w_clouds;
		  w_temperature = max 0 weather.w_temperature;
		}


let update ~new_weather ~current_weather ~player ~climate ~indoors ~current_tod =
	let new_tod = Weather.sun () in

	let new_weather = adjust_weather ~player ~climate new_weather in

	let wind_check = current_weather.w_wind <> new_weather.w_wind in
	let rain_check = current_weather.w_rain <> new_weather.w_rain in
	let clouds_check = current_weather.w_clouds <> new_weather.w_clouds in
	let temperature_check = current_weather.w_temperature <> 
		new_weather.w_temperature
	in
	let tod_check = current_tod <> new_tod
	in		
		match 
			wind_check, rain_check, clouds_check, 
			temperature_check, tod_check
		with
			| true, _, _, _, _ -> 
				  new_wind ~player ~indoors ~current_weather ~new_weather ~climate
			| false, true, _, _, _ -> 
				  new_rain ~player ~indoors ~current_weather ~new_weather ~climate
			| false, false, true, _, _ ->
				  new_clouds ~player ~indoors ~current_weather ~new_weather

			| false, false, false, true, _ ->
				  new_temperature ~player ~indoors ~current_weather ~new_weather

			| false, false, false, false, true ->
				  new_time_of_day ~player ~new_weather ~climate ~current_tod ~new_tod
			| _, _, _, _, _ ->
				  ()


let update new_weather player =
	let current_tod = Model.Props.get_time_of_day player in
	let current_weather = Model.Props.get_local_weather player in

	let terrain = Model.Props.get_terrain (Model.Tree.parent player) in

		if has_weather terrain
		then (
			let climate = climate terrain in
			let indoors = is_indoors terrain in
				update ~new_weather ~current_weather ~player ~climate ~indoors ~current_tod
		)
