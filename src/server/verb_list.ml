(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Verbs
open Parser


let unknown_verb = {
	v_name = " !unknown_command";
	v_frame = Unary unknown_command;
	v_args = [];
}

let vv = [

	{ v_name = "look" ; 
      v_frame = Unary look;
	  v_args = [];			
	};

	{ v_name = "take" ; 
      v_frame = Binary take;
	  v_args = [Var (Patient, ItemInLocation)];			
	};

	{ v_name = "drop" ;
      v_frame = Binary drop;
	  v_args = [Var (Patient, ItemCarried)];			
	 };

  (* temporary hack *)
	{ v_name = "pdrop" ;
      v_frame = Binary pdrop;
	  v_args = [Var (Patient, ItemCarried)];			
	};

	{ v_name = "exits" ;
      v_frame = Unary exits;
	  v_args = [];			
	 };

	{ v_name = "quit" ;
      v_frame = Unary quit;
	  v_args = [];			
	 };
(*
	{ v_name = "help" ;
      v_frame = Unary help;
	  v_args = [];			
	 };
*)
	{ v_name = "inventory" ;
      v_frame = Unary inventory;
	  v_args = [];			
	 };

	{ v_name = "examine" ;
      v_frame = Binary examine;
	  v_args = [Var (Patient, ItemPresent)];			
	 };

	{ v_name = "close" ;
      v_frame = Binary close_ap;
	  v_args = [Var (Patient, ItemOrPortalPresent)];			
	};
	
	{ v_name = "open" ;
      v_frame = Binary open_ap;
	  v_args = [Var (Patient, ItemOrPortalPresent)];			
	};

	{ v_name = "scry" ;
	  v_frame = Binary scry_ap;
	  v_args = [Var (Patient, ItemOrPortalPresent)];			
	};
	
	{ v_name = "score" ;
      v_frame = Unary score;
	  v_args = [];			
	};
	
	{ v_name = "cheat" ;
	  v_frame = Unary cheat;
	  v_args = [];			
	};
	(*
	{ v_name = "whentick" ;
	  v_frame = Unary when_next;
	  v_args = [];			
	};
	*)
	{ v_name = "probe" ;
      v_frame = Binary probe;
	  v_args = [Var (Patient, ItemPresent)];			
	}; (* present or held? *)
	
	{ v_name = "gamestat";
	  v_frame = Unary gamestat;
	  v_args = [];			
	};

	{ v_name = "teleport";
	  v_frame = Binary teleport;
	  v_args = [Var (Patient, LocationCode)];			
	};

	{ v_name = "unlock";
	  v_frame = Ditrans unlock;
	  v_args = [ Var (Patient, ItemOrPortalPresent);
				 Constant "with";
				 Var (Instrument, ItemCarried); ];
	};
	
	{ v_name = "lock";
	  v_frame = Ditrans lock;
	  v_args = [ Var (Patient, ItemOrPortalPresent);
				 Constant "with";
				 Var (Instrument, ItemCarried); ];
	};

	{ v_name = "mstatus";
	  v_frame = Binary mstatus;
	  v_args = [ Var (Patient, MonsterPresent); ]
	};

	{ v_name = "superget";
	  v_frame = Binary superget;
	  v_args = [ Var (Patient, ItemAnywhere); ];
	};

	{ v_name = "drive";
	  v_frame = Binary drive;
	  v_args = [ Var (Patient, ItemCarried); ];
	};

	{ v_name = "undrive";
	  v_frame = Unary undrive;
	  v_args = [];
	};

	{ v_name = "wizmode";
	  v_frame = Unary wizmode;
	  v_args = [];
	};

	{ v_name = "unwizmode";
	  v_frame = Unary unwizmode;
	  v_args = [];
	};

	{ v_name = "delay";
	  v_frame = Unary delay;
	  v_args = [];
	};

	{ v_name = "whereis";
	  v_frame = Binary_word whereis;
	  v_args = [ Var (Patient, Literal); ];
	};

	{ v_name = "locate";
	  v_frame = Binary_word locate;
	  v_args = [ Var (Patient, Literal); ];
	};

    { v_name = "say";
	  v_frame = Binary_word say;
	  v_args = [ Rest_of_line (Patient, Literal); ];
	};

    { v_name = "emote";
	  v_frame = Binary_word emote;
	  v_args = [ Rest_of_line (Patient, Literal); ];
	};

    { v_name = "shout";
	  v_frame = Binary_word shout;
	  v_args = [ Rest_of_line (Patient, Literal); ];
	};

	{ v_name = "tell";
	  v_frame = Ditrans_word tell;
      v_args = [ Var (Patient, CurrentPlayer);
				 Rest_of_line (Instrument, Literal); ];
	};

	{ v_name = "quest";
	  v_frame = Unary start_quest;
	  v_args = [];
	};

	{ v_name = "unquest";
	  v_frame = Unary stop_quest;
	  v_args = [];
	};

	(* debug *)
	{ v_name = "winquest";
	  v_frame = Unary finish_quest;
	  v_args = [];
	};

	{ v_name = "status";
	  v_frame = Unary status;
	  v_args = [];
	};

	{ v_name = "kill";
	  v_frame = Binary attack;
	  v_args = [ Var (Patient, PlayerPresent); ]
	};

	{ v_name = "fight";
	  v_frame = Binary_word fight;
	  v_args = [ Var (Patient, Literal); ]
	};

	{ v_name = "burrow";
	  v_frame = Ditrans_word addlink;
	  v_args = [ Var (Instrument, Literal);
				 Constant "to";
				 Var (Patient, LocationCode); 
			   ]
	};

	{ v_name = "collapse";
	  v_frame = Binary_word collapse;
	  v_args = [ Var (Patient, Literal); ];
	};

	{ v_name = "light" ;
      v_frame = Binary switch_on;
	  v_args = [Var (Patient, ItemCarried)];			
	 };

	{ v_name = "douse" ;
      v_frame = Binary switch_off;
	  v_args = [Var (Patient, ItemCarried)];			
	 };

	{ v_name = "roomgraph";
	  v_frame = Unary room_graph;
	  v_args = [];
	};

	{ v_name = "buy";
	  v_frame = Binary_word buy_spell;
	  v_args = [ Var (Patient, Literal); ]
	};

	{ v_name = "die";
	  v_frame = Unary do_death;
	  v_args = [];
	};

	{ v_name = "disable";
	  v_frame = Binary unset_trap;
	  v_args = [Var (Patient, ItemOrPortalPresent);];
	};

	{ v_name = "trap";
	  v_frame = Binary set_trap;
	  v_args = [Var (Patient, ItemOrPortalPresent);];
	};
	
	{ v_name = "who";
	  v_frame = Unary list_players;
	  v_args = [];
	};
	
	{ v_name = "people";
	  v_frame = Unary list_people;
	  v_args = [];
	};

	{ v_name = "insert";
	  v_frame = Ditrans insert;
	  v_args = [ Var (Instrument, ItemCarried);
				 Constant "into";
				 Var (Patient, ItemPresent); ];
	};

	{ v_name = "extract";
	  v_frame = Ditrans_word extract;
	  v_args = [ Var (Instrument, Literal);
				 Constant "from";
				 Var (Patient, ItemPresent); ];
	};

	{ v_name = "search";
	  v_frame = Binary search_in_object;
	  v_args = [Var (Patient, ItemPresent);];
	};

	{ v_name = "join";
	  v_frame = Binary_word join_guild;
	  v_args = [Var (Patient, Literal);];
	};

	{ v_name = "shutdown";
	  v_frame = Unary run_shutdown;
	  v_args = [];
	};

	{ v_name = "exterminate"; (* test verb *)
	  v_frame = Binary exterminate;
	  v_args = [ Var (Patient, MonsterPresent); ]
	};

]
