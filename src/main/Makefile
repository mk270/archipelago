# Oddly enough, you need to type "make bc" to compile(!)

# Preparation:
#
# apt-get install ocamlmakefile
# ln -s /usr/share/ocamlmakefile/OCamlMakefile ./OCamlMakefile

include common.mk

SOURCES := $(COMMON_SOURCES) $(TEST_SOURCES) main.ml
#SOURCES := $(COMMON_SOURCES) main.ml

RESULT  := ap1

all: byte-code

include OCamlMakefile
