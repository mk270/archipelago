# Oddly enough, you need to type "make bc" to compile(!)

# Preparation:
#
# apt-get install ocamlmakefile
# ln -s /usr/share/ocamlmakefile/OCamlMakefile ./OCamlMakefile

include common.mk

SOURCES := $(COMMON_SOURCES)

RESULT  := mudmodel

all: byte-code-library

include OCamlMakefile

