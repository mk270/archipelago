# Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan
#
# Copyright (C) 2009-2012  Martin Keegan
#
# This programme is free software; you may redistribute and/or modify
# it under the terms of the GNU Affero General Public Licence as published by
# the Free Software Foundation, either version 3 of said Licence, or
# (at your option) any later version.


__all__ = [ 'formatList', 'allPlayers', 'unique', 'uniqueK', 'ucfirst',
            'allPlayersExcept' ]

def ucfirst(s):
    return s[0].upper() + s[1:]

def formatList(l):
    if len(l) == 0:
        raise ValueError
    elif len(l) == 1:
        return l[0]
    elif len(l) == 2:
        return "%s and %s" % (l[0], l[1])
    else:
        head = l[:-2]
        tail = l[-2:]
        return "%s, %s" % (
            ", ".join(head), 
            formatList(tail)
            )

def allPlayers(multi):
    for s in multi.allConnections():
        if not hasattr(s, 'isLoggedIn'):
            continue
        if s.isLoggedIn():
            yield s.player
    raise StopIteration

def allPlayersExcept(p):
    for s in filter(lambda x: x is not p, allPlayers(p.socket.multi)):
        yield s
    raise StopIteration

def unique(iterable):
    seen = set()
    seen_add = seen.add
    for el in iterable:
        if el not in seen:
            seen_add(el)
            yield el
    raise StopIteration


def uniqueK(iterable, key):
    seen = set()
    seen_add = seen.add
    for el in iterable:
        k = key(el)
        if k not in seen:
            seen_add(k)
            yield el
    raise StopIteration
