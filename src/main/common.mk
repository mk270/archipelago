
OCAMLFLAGS := -rectypes -g -w Aez -warn-error Aez
#OCAMLFLAGS := -rectypes -g -dtypes
OCAMLLDFLAGS := -g
ANNOTATE := yes
INCDIRS := ../utils ../model ../server

PACKS := sqlite3 oUnit
LIBS := unix str general mudmodel mudserver

COMMON_SOURCES := \


TEST_SOURCES := \
	test_prioq.ml \
	test_movement.ml \
	test_all.ml 