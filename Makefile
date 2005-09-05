
all:
	gnatmake -gnatwclpr -gnatf main -largs -L/usr/X11R6/lib -lX11

clean:
	rm -f *.o *.ali main

