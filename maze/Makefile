
ADACFLAGS = -O2 -gnatp -gnatf -A../adainclude -aL../lib
ADALFLAGS = -L/usr/X11R6/lib -lX11 -lXft -lfreetype -lfontconfig -lXrender -labfx

maze: maze.adb
	gnatmake $(ADACFLAGS) maze -o maze -largs $(ADALFLAGS)

clean:
	rm -f *.o *.ali maze

run: maze
	LD_LIBRARY_PATH=../lib ./maze

