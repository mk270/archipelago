# Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan
#
# Copyright (C) 2009-2012  Martin Keegan
#
# This programme is free software; you may redistribute and/or modify
# it under the terms of the GNU Affero General Public Licence as published by
# the Free Software Foundation, either version 3 of said Licence, or
# (at your option) any later version.


__all__ = [ 'Listener', 'StatusSocket' ]

import re
import socket
from Util import allPlayers

class Listener(object):
    def __init__(self, port, sockType):
        self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.s.setblocking(0)
        self.s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.s.bind(('', port))
        self.s.listen(5)
        self.multi = None
        self.sockType = sockType
        self.isListener = True

    def storeMultiplexer(self, m):
        self.multi = m

    def handleReadable(self):
        (conn, address) = self.s.accept()
        c = self.sockType(conn, address)
        c.setNonBlocking()
        self.multi.addSocket(c)
        c.enter()

class Socket(object):
    def __init__(self, s, addr):
        self.s = s
        self.address = addr
        self.input_buffer = ""
        self.output_buffer = ""
        self.multi = None

    def __del__(self):
        pass

    def setNonBlocking(self):
        self.s.setblocking(0)

    def handleWritable(self):
        pass

    def handleReadable(self):
        l = len(self.input_buffer)
        self.input_buffer += self.s.recv(4096)
        if len(self.input_buffer) == l:
            self.EOF()
        else:
            self.newInput()

    def newInput(self):
        pass

    def EOF(self):
        self.multi.removeSocket(self)
        self.s.close()
                
    def storeMultiplexer(self, multi):
        self.multi = multi

    def emit(self, line):
        self.s.send(line + "\r\n")

    def emit_raw(self, data):
        self.s.send(data)

    def passwordMode(self):
        self.s.send("\377PSW \r\n")

    def emitNOOP(self):
        self.emit("\377NOP")

    def enter(self):
        pass

zap_cr = re.compile(r'\r')

class LineSocket(Socket):
    def __init__(self, s, addr):
        super(LineSocket, self).__init__(s, addr)

    def handleLine(self, line):
        print "Line: %s" % line

    def newInput(self):
        self.input_buffer = zap_cr.sub('', self.input_buffer, 0)

        y = self.input_buffer.rfind("\n")
        if y == -1:
            return
        to_process = self.input_buffer[0:y]
        self.input_buffer = self.input_buffer[y+1:]
        for i in to_process.split("\n"):
            self.handleLine(i)

class StatusSocket(LineSocket):
    def __init__(self, s, addr):
        super(self.__class__, self).__init__(s, addr)

    def enter(self):
        self.status()
        self.EOF()

    def status(self):
        self.emit("\r\nCurrently connected:\r\n")
        [ self.emit(p.unvague()) for p in allPlayers(self.multi) ]

