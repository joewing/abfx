
ADACFLAGS = -O2 -gnatp -gnatf -A../adainclude -aL../lib
ADALFLAGS = -L/usr/X11R6/lib -lX11 -ltabfx

all:
	gnatmake $(ADACFLAGS) main -o calc -largs $(ADALFLAGS)

clean:
	rm -f *.o *.ali calc

