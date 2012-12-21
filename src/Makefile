
all: main/ap1

utils/general.cma:
	$(MAKE) -C utils

model/mudmodel.cma:
	$(MAKE) -C model

server/mudserver.cma:
	$(MAKE) -C server

main/ap1: utils/general.cma model/mudmodel.cma server/mudserver.cma
	$(MAKE) -C main

.PHONY: clean
clean:
	$(MAKE) -C utils clean
	$(MAKE) -C model clean
	$(MAKE) -C server clean
	$(MAKE) -C main clean
