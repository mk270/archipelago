(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Socket
(*
grep def Multiplexer.py
    def __init__(self):
    def addSocket(self, s):
    def removeSocket(self, s):
    def start(self):
    def stop(self):
    def run(self):
    def block(self):
    def shutdown(self):
    def allConnections(self):
*)

type multiplexer

val create : unit -> multiplexer

val add_socket : multiplexer -> socket -> unit

val start : multiplexer -> unit
val stop  : multiplexer -> unit
val run   : multiplexer -> unit

val add_timeout_callback : multiplexer -> (unit -> unit) -> unit
