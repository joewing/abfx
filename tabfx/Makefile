
ADACFLAGS = -O2 -gnatp -gnatf -fPIC
ADALFLAGS = -L/usr/X11R6/lib -lX11

all:
	mkdir -p ../lib
	mkdir -p ../adainclude
	gnatmake $(ADACFLAGS) -c lib -largs $(ADALFLAGS)
	rm lib.o lib.ali
	gcc -shared -o ../lib/libtabfx.so *.o
	cp *.ali ../adainclude
	cp *.ads ../adainclude

clean:
	rm -f *.ali *.o
	rm -fr ../lib ../adainclude


