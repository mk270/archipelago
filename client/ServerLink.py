
# Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan
#
# Copyright (C) 2009-2012  Martin Keegan
#
# This programme is free software; you may redistribute and/or modify
# it under the terms of the GNU Affero General Public Licence as published by
# the Free Software Foundation, either version 3 of said Licence, or
# (at your option) any later version.

import socket
import sys

from Networking import Socket, zap_cr

class ServerLink(Socket):
    def __init__(self, address, ss=None):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        super(ServerLink, self).__init__(sock, address)
        self.ss = ss
        self.ss.set_sock(self)
        self.set_hostname()
        self.connection_failed = self.connect(address)
        self.setNonBlocking()
        self.delim = self.get_delim()
        self.setup()

    def set_hostname(self):
        if len(sys.argv) == 2:
            progname, ip_address = sys.argv
            
            self.ip_address = ip_address
            
            try:
                self.hostname, _ = socket.gethostbyaddr(ip_address)
            except:
                self.hostname = ip_address
        else:
            self.hostname = None
            self.ip_address = "127.0.0.1"


    def connect(self, address):
        try:
            self.s.connect(address)
            return None
        except Exception as e:
            return str(e)

    def newInput(self):
        self.input_buffer = zap_cr.sub('', self.input_buffer, 0)

        y = self.input_buffer.rfind(self.delim)
        if y == -1:
            return
        to_process = self.input_buffer[0:y]
        self.input_buffer = self.input_buffer[y+1:]
        for i in to_process.split(self.delim):
            self.handleLine(i)

    def handleHangup(self):
        self.ss.emit_line("Remote hung up")
        ## EOF?

    def handleLine(self, line):
        if len(line) >= 4 and line[0] == '\377':
            cmd = line[1:4]
            arg = line[4:]
            self.ss.process_srv_command(cmd, arg)
        else:
            self.ss.emit_line(line)
 
    def EOF(self):
        self.multi.removeSocket(self)
        self.multi.stop()
        self.s.close()
        self.ss.shutdown()
        self.ss.emit_line("Connection closed by foreign host.")

    def get_delim(self):
        return "\n"

    def setup(pass):
        pass
