(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Name

(*
  let delay_base = 0.5
  let delay_random = 3.0
*)
let delay_base = 0.1
let delay_random = 0.1

let onechar s =
	let s = String.capitalize s in
		s.[0] = 'Y'

let disconnect sess =
	Session.end_session sess

let censor_password = "\xffPSW\r\n"

let enter_message = function
	| 2 -> "Your avatar passes over the ocean, seeking a fresh body.\r\n"
	| 1 -> "The mists drift apart, but all you can sense is a grey, formless void ...\r\n"
	| 0 -> "With a sudden snap your vision returns, though your eyes were already open.\r\n"
	| _ -> failwith "missing enter msg"

let state_greeting_msg = function
	| Session.UnAuthenticated -> Some "Please enter your name.\r\n";
	| Session.GetPass -> Some ("Please enter your password.\r\n" ^ censor_password)
	| Session.GetSex -> Some "Please enter your sex (M/F).\r\n"
	| Session.NewPass1 -> Some ("Please enter a password.\r\n" ^ censor_password)
	| Session.NewPass2 -> Some ("Please re-enter this password.\r\n" ^ censor_password)
	| Session.LoggedIn _ -> Some "Welcome!\r\n\r\n"
	| Session.NewConnection -> Some "MOTD goes here.\r\n"
	| Session.ConfirmName name -> Some ("Did I get the name right, " ^ 
									   (name)^ "?\r\n")
	| Session.Entering (_, i) ->
		let delay = (delay_base +. Random.float delay_random) in
		let d = Delay.do_delay_crlf ~delay in
			Some ((enter_message i) ^ d)
	| Session.LoggedOut -> None

let exit_state_message = function
	| Session.ConfirmName _ -> Some "Ok, well try again ...\r\n"
	| _ -> None

let rec transition_state sess state = 
	let old_state = Session.get_state sess in
		(match exit_state_message old_state with
			| None -> ()
			| Some msg -> Session.emit sess msg);
		Session.set_state sess state;
		(match state_greeting_msg state with
			| None -> ()
			| Some msg -> Session.emit sess msg);
		enter_state sess state

and enter_state sess state =
	match state with
		| Session.NewConnection -> 
			transition_state sess Session.UnAuthenticated
		| Session.LoggedOut -> 
			disconnect sess
		| Session.LoggedIn actor ->
			Model.Tree.insert_into ~recipient:(World.get_start_room ()) actor;
			Verbs.look ~actor;
			Model.post_event (Model.Logon actor)
		| _ -> ()

and login sess name =
	let p = Model.Create.create_player (name, NoAdam, Singular) in
		transition_state sess (Session.LoggedIn p)


let enter sess name =
	transition_state sess (Session.Entering (name, 2))

let logout sess p =
	(*	Model.Tree.remove_from (Model.Tree.parent p) p; *)
	Model.destroy p;
	Model.post_event (Model.Logout p);
	transition_state sess Session.LoggedOut

let handle_login sess line =
	if Passwd.sensible_name line 
	then (
		(*		if not (Passwd.name_too_similar ~name:line)
				then ( *)
		let n = Passwd.normalise_name line in
			Session.set_name sess n;
			if Passwd.player_name_known n
			then transition_state sess Session.GetPass
			else transition_state sess (Session.ConfirmName n)
	(*		  else emit sess "That name is too similar to someone else's.\r\n" *)
	)
		  else
			Session.emit sess "That's not a sensible name.\r\n"

let get_new_password sess p =
	if Passwd.sensible_password p 
	then (Session.set_password sess p;
		  transition_state sess Session.NewPass2)
	else Session.emit sess "That's not a sensible password.\r\n"

let confirm_new_password sess p =
	if Session.get_password sess = p 
	then enter sess (Session.get_name sess)
	else
		( Session.emit sess "Passwords don't match.\r\n";
		  transition_state sess Session.NewPass1 )

let get_password sess p =
	let name = Session.get_name sess in
		if Passwd.password_matches name p 
		then (* login sess name *)
			enter sess name
		else
			(Session.emit sess "Wrong password!\r\n";
			 transition_state sess Session.UnAuthenticated)

let player_new_line p line =
	Commands.parse_line p line

let get_sex sess line =
	try let sex = Sex.sex_of_string line in
			ignore(sex);
			transition_state sess Session.NewPass1
	with _ ->
		transition_state sess Session.GetSex

let handle_line sess line = function
	| Session.LoggedIn p -> player_new_line p line
	| Session.UnAuthenticated -> handle_login sess line
	| Session.GetPass -> get_password sess line
	| Session.ConfirmName _ -> 
		if (onechar line) 
		then transition_state sess Session.GetSex
		else transition_state sess Session.UnAuthenticated
	| Session.GetSex -> get_sex sess line
	| Session.NewPass1 -> get_new_password sess line
	| Session.NewPass2 -> confirm_new_password sess line
	| Session.LoggedOut -> disconnect sess
	| Session.NewConnection -> Session.emit sess "aaargh\r\n"
	| Session.Entering (name, i) -> 
		(if i > 0
		 then transition_state sess (Session.Entering (name, i - 1))
		 else login sess name)

let handle_blank_line sess = function
	| Session.UnAuthenticated -> Session.emit sess "Hello? Who is that?\r\n"
	| Session.GetPass -> Session.emit sess "Empty password?\r\n"
	| Session.NewPass1 -> Session.emit sess "Blank passwords not allowed!\r\n"
	| Session.LoggedIn p -> player_new_line p ""
	| Session.ConfirmName _ -> transition_state sess Session.UnAuthenticated
	| Session.NewConnection
	| Session.GetSex
	| Session.LoggedOut
	| Session.NewPass2 -> Session.emit sess "What?\r\n"
	| Session.Entering (name, i) -> 
		(if i > 0
		 then transition_state sess (Session.Entering (name, i - 1))
		 else login sess name)

let new_line s line =
	let sess = Session.get_session s in
	let st = Session.get_state sess in
		if String.length line = 0
		then handle_blank_line sess st
		else handle_line sess line st

let hangup s = 
	(* we need to check if the player is logged in, and if so, do logout *)
	let session = Session.get_session s in
	let state = Session.get_state session in
		match state with
			| Session.LoggedIn p -> logout session p
			| _ -> transition_state session Session.LoggedOut

type game_proto_msg =
	| NewLine of Socket.socket * string
	| Hangup of Socket.socket
	| Logout of Session.session * Model.mudobject

let dispatch = function
	| NewLine (sock, s) -> new_line sock s
	| Hangup sock -> hangup sock
	| Logout (sess, p) -> logout sess p
