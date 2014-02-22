# Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan
#
# Copyright (C) 2009-2012  Martin Keegan
#
# This programme is free software; you may redistribute and/or modify
# it under the terms of the GNU Affero General Public Licence as published by
# the Free Software Foundation, either version 3 of said Licence, or
# (at your option) any later version.


import select

__all__ = [ 'Multiplexer' ]

class Multiplexer:
    def __init__(self):
        self.p = select.poll()
        self.sockets = []
        self.switchboard = {}
        self.running = False
        self.timeout = 5000
        self.onTimeout = lambda : None

    def addSocket(self, s):
        self.p.register(s.s, select.POLLIN)
        self.sockets.append(s.s)
        fd = s.s.fileno()
        self.switchboard[fd] = s
        s.storeMultiplexer(self)

    def removeSocket(self, s):
        self.p.unregister(s.s)
        fd = s.s.fileno()
        del(self.switchboard[fd])
        self.sockets.remove(s.s)	

    def start(self):
        self.running = True

    def stop(self):
        self.running = False

    def run(self):
        self.start()

        while self.running:
            self.block()
            self.onTimeout()

    def block(self):
        rv = self.p.poll(self.timeout)
        for active in rv:
            fd = active[0]
            events = active[1]

            if events & select.POLLIN:
                sock = self.switchboard[fd]
                sock.handleReadable()
	    if events & select.POLLHUP:
                sock = self.switchboard[fd]
                sock.handleHangup()
			
    def shutdown(self):
        [ s.EOF() for s in self.allConnections() ]
	self.stop()

    def allConnections(self):
        for s in self.sockets:
            c = self.switchboard[s.fileno()]
            if hasattr(c, 'isListener'):
                continue
            yield c
        raise StopIteration

    def setTimeout(self, timeout):
        self.timeout = timeout

    def setOnTimeout(self, fn):
        self.onTimeout = fn
    
    def clearOnTimeout(self):
        self.onTimeout = lambda : None
