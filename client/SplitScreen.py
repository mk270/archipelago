
# Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan
#
# Copyright (C) 2009-2012  Martin Keegan
#
# This programme is free software; you may redistribute and/or modify
# it under the terms of the GNU Affero General Public Licence as published by
# the Free Software Foundation, either version 3 of said Licence, or
# (at your option) any later version.

import curses
import curses.ascii
import time

class Throbber(object):
    def __init__(self):
        self.text = '/|\\-'
        self.state = 0
    def next(self):
        self.state += 1
        self.state %= len(self.text)
        return self.text[self.state]


class SplitScreen(object):
    HIST_DEPTH = 30

    def __init__(self, scr):
        self.scr = scr
        self.sock = None
        self.prompt = '> '
        self.new_cmd()
        self.history = [""] * self.HIST_DEPTH
        self.hist_pos = 0
        self.cur_hist_depth = 1
        self.bottom_line = self.get_screen_height()
        self.password_mode = False
        self.server_link = None
        self.delay_until = None
        self.throbber = Throbber()

    def get_screen_height(self):
        y = self.scr.getmaxyx()[0] - 1
        if y < 5:
            y = 5
        elif y > 55:
            y = 55
        return y

    def throb(self):
        self.scr.move(self.bottom_line, 0)
        self.scr.clrtoeol()
        s = self.throbber.next() + " "
        self.scr.addstr(self.bottom_line, 0, s)
        self.scr.refresh()
        now = time.time()
        if now >= self.delay_until:
            self.stop_delay()

    def set_server_link(self, server_link):
        self.server_link = server_link

    def new_cmd(self):
        self.cmd = ''
        self.pos = 0

    def set_sock(self, sock):
        self.sock = sock

    def setup(self):
        self.scr.idlok(True)
        self.scr.scrollok(True)
        self.scr.setscrreg(0, self.bottom_line - 1)
        self.scr.nodelay(True)

    def fixup_line(self, line):
        return line

    def emit_line(self, line):
        self.scr.scroll(1)
        line = self.fixup_line(line)
        self.scr.addstr(self.bottom_line - 1, 0, line)
        self.place_cursor()
        self.scr.refresh()

    def write_prompt(self):
        self.scr.addstr(self.bottom_line, 0, self.prompt)
        self.scr.refresh()

    def place_cursor(self):
        self.scr.move(self.bottom_line, len(self.prompt) + self.pos)

    def clear_input(self):
        self.new_cmd()
        self.place_cursor()
        self.scr.clrtoeol()

    def insert_char(self, ch):
        if not self.password_mode:
            c = ch
        else:
            c = '*'
        self.scr.addch(self.bottom_line, len(self.prompt) + self.pos, c)
        self.cmd = self.cmd[:self.pos] + chr(ch) + self.cmd[self.pos:]
        self.pos += 1
        if self.pos < len(self.cmd):
            self.scr.addstr(self.bottom_line, len(self.prompt) + self.pos, 
                            self.cmd[self.pos:])
            self.scr.move(self.bottom_line, len(self.prompt) + self.pos)

    def adjust_after_deletion(self):
        self.scr.move(self.bottom_line, len(self.prompt) + self.pos)
        if self.pos < len(self.cmd):
            self.scr.addstr(self.bottom_line, len(self.prompt) + self.pos, 
                            self.cmd[self.pos:])
            self.scr.clrtoeol()
            self.scr.move(self.bottom_line, len(self.prompt) + self.pos)
        else:
            self.scr.clrtoeol()

    def backspace(self):
        if self.pos == 0:
            return
        self.cmd = self.cmd[:self.pos - 1] + self.cmd[self.pos:]
        self.pos -= 1
        self.adjust_after_deletion()

    def delete(self):
        if len(self.cmd) < 1:
            return
        if self.pos == len(self.cmd):
            return
        self.cmd = self.cmd[:self.pos] + self.cmd[self.pos + 1:]
        self.adjust_after_deletion()

    def record_history(self, cmd):
        self.history[self.hist_pos] = cmd
        self.hist_pos += 1
        if self.cur_hist_depth < self.HIST_DEPTH:
            self.cur_hist_depth += 1
        self.hist_pos %= self.cur_hist_depth

    def send_line(self):
        if self.password_mode:
            line = self.prompt + "*" * len(self.cmd)
        else:
            line = self.prompt + self.cmd
        self.emit_line(line)
        self.sock.emit(self.cmd)

        if not self.password_mode:
            self.record_history(self.cmd)

        self.clear_input()
        self.password_mode = False

    def replace_line(self, s):
        self.clear_input()
        self.scr.addstr(self.bottom_line, len(self.prompt), s)
        self.cmd = s
        self.pos = len(s)

    def kill_line(self):
        self.replace_line("")

    def do_history(self, d):
        if self.password_mode:
            return
        self.hist_pos += d + self.cur_hist_depth
        self.hist_pos %= self.cur_hist_depth
        self.replace_line(self.history[self.hist_pos])

    def noop(self):
        self.emit_line("[Not implemented]")

    def history_back(self):
        self.do_history(-1)

    def history_fwd(self):
        self.do_history(1)

    def go_left(self):
        if self.pos > 0:
            self.pos -= 1
            self.scr.move(self.bottom_line, len(self.prompt) + self.pos)

    def go_right(self):
        if self.pos < len(self.cmd):
            self.pos += 1
            self.scr.move(self.bottom_line, len(self.prompt) + self.pos)

    def handle_input(self):
        # todo: ctrl-k, ctrl-d, make backspace work mid-word
        CTRL_U = 21
        CTRL_D = 4
        CTRL_K = 11
        handlers = {
            curses.KEY_UP: self.history_back,
            curses.KEY_DOWN: self.history_fwd,
            curses.KEY_LEFT: self.go_left,
            curses.KEY_RIGHT: self.go_right,
            curses.KEY_BACKSPACE: self.backspace,
            CTRL_D: self.delete,
            CTRL_U: self.kill_line,
            0: lambda : None
            }

        ch = self.scr.getch() ## FIXME -1?

        if self.delay_until:
            now = time.time()
            if now < self.delay_until:
                return
            self.stop_delay()

        if ch >= 32 and ch <= 126:
            self.insert_char(ch)
        elif ch == curses.ascii.LF:
            self.send_line()
        elif ch in handlers:
            handlers[ch]()
        else:
            self.emit_line("[Key uncaught: %d]" % ch)

        #self.scr.addstr(5,5,str(ch))
        self.scr.refresh()

    def stop_delay(self):
        self.delay_until = None
        self.server_link.multi.setTimeout(5000)
        self.server_link.multi.clearOnTimeout()
        self.write_prompt()
        self.replace_line(self.cmd)
        self.sock.emit("") # cheekily notify the server we're done
        
    def delay(self, delay):
        now = time.time()
        self.delay_until = now + delay
        self.server_link.multi.setTimeout(250)
        self.server_link.multi.setOnTimeout(lambda : self.throb())

    def set_prompt(self, new_prompt):
        self.prompt = new_prompt
        self.write_prompt()
        self.replace_line(self.cmd)

    def set_password_mode(self, mode):
        self.password_mode = mode

    def process_srv_command(self, cmd, arg):
        if cmd == 'PSW':
            self.set_password_mode(True)
        elif cmd == 'PRN':
            self.set_prompt(arg)
        elif cmd == 'DLY':
            try:
                delay = float(arg)
            except:
                delay = 0.0
            self.delay(delay)
        else:
            self.emit_line("CMD: [%s]" % cmd)
            self.emit_line("ARG: [%s]" % arg)

    def shutdown(self):
        self.set_prompt("")
