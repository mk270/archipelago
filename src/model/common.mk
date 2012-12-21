
OCAMLFLAGS := -rectypes -g -w Aez -warn-error Aez
#OCAMLFLAGS := -rectypes -g -dtypes
OCAMLLDFLAGS := -g
ANNOTATE := yes
INCDIRS := ../utils

PACKS := sqlite3 oUnit
LIBS := unix str general

COMMON_SOURCES := \
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
	model.ml
