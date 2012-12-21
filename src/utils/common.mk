
OCAMLFLAGS := -rectypes -g -w Aez -warn-error Aez
#OCAMLFLAGS := -rectypes -g -dtypes
OCAMLLDFLAGS := -g
ANNOTATE := yes

PACKS := sqlite3 oUnit
LIBS := unix str

COMMON_SOURCES := \
	utils.ml \
	fsm.ml \
	name.ml \
	container.ml \
	direction.ml \
	sex.ml \
	light.ml \
	light_fsm.ml \
	vehicle.ml \
	item_prop.ml \
	loc_code.ml \
	terrain.ml \
	guild.ml \
	player_state.ml \
	persona.ml \
	weather.ml \
	combat_state.ml \
	aperture.ml \
	sentence.ml \
	model.ml \
	world.ml \
	networking.ml \
	socket.ml \
	reset.ml \
	prioqueue.ml \
	workqueue.ml \
	multiplexer.ml \
	lexicon.ml \
	grammar.ml \
	weffect.ml \
	monster.ml \
	events.ml \
	daemon.ml \
	atmosphere.ml \
	fights.ml \
	delay.ml \
	leaving.ml \
	verbs.ml \
	parser.ml \
	verb_list.ml \
	commands.ml \
	passwd.ml \
	game_protocol.ml \
	connection.ml \
	server.ml

TEST_SOURCES := \
	test_prioq.ml \
	test_movement.ml \
	test_all.ml 