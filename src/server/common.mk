
OCAMLFLAGS := -rectypes -g -w Aez -warn-error Aez
#OCAMLFLAGS := -rectypes -g -dtypes
OCAMLLDFLAGS := -g
ANNOTATE := yes
INCDIRS := ../utils ../model

PACKS := sqlite3
LIBS := mudmodel

COMMON_SOURCES := \
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
