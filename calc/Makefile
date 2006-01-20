
ADACFLAGS = -O2 -gnatp -gnatf
X11LFLAGS = -L/usr/X11R6/lib -lX11
TABFXFLAGS = -aL../objects -aO../objects -aI../include

all:
	gnatmake $(ADACFLAGS) main -o calc $(TABFXFLAGS) -largs $(X11LFLAGS)

clean:
	rm -f *.o *.ali calc

