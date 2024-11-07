#!/usr/bin/python3
import curses
import atexit
import time

stdscr = None


def start_cursing():
	global stdscr
	if stdscr == None:
		try:
			stdscr = curses.initscr()
		except curses.error as e:
			print(f"Can't curse here. {e}")
			exit(1)
		curses.noecho()  # turn off automatic echoing
		curses.cbreak()  # react to keys instantly, without enter
		stdscr.keypad(True)

def done_cursing():
	global stdscr

if stdscr != None:
	curses.nocbreak()
	stdscr.keypad(False)
	curses.echo()
	stdscr = None

atexit.register(done_cursing)

def main() -> None:
	start_cursing()
	begin_x = 20;
	begin_y = 7
	height = 5;
	width = 40
	win = curses.newwin(height, width, begin_y, begin_x)
	time.sleep(10)
	done_cursing()

if __name__ == '__main__':
	main()
