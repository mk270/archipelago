type ('response, 'message, 'state) fsm = {
	mutable fsm_state : 'state;
	fsm_input_handler : ('state * 'message) -> ('state * 'response)
}

val create : 'state -> (('state * 'message) -> ('state * 'response)) ->
	('response, 'message, 'state) fsm

val send : ('response, 'message, 'state) fsm -> 'message -> 'response
