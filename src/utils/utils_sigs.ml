module type Fsm = sig
type ('response, 'message, 'state) fsm = {
	mutable fsm_state : 'state;
	fsm_input_handler : ('state * 'message) -> ('state * 'response)
}

val create : 'state -> (('state * 'message) -> ('state * 'response)) ->
	('response, 'message, 'state) fsm

val send : ('response, 'message, 'state) fsm -> 'message -> 'response
end

module type Ext = sig

val remove : 'a -> 'a list -> 'a list

val comma_sep : string list -> string

val initial_match : string -> string -> bool

val random_list_member : 'a list -> 'a

val guard_exception : exn -> unit

val tally : 'a list -> ('a * int) list

end
