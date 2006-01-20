
ADAC = gnatgcc
ADABIND = gnatbind
ADALINK = gnatlink

ADACFLAGS = -O2 -gnatp -gnatf -Iinclude
ADABFLAGS =
ADALFLAGS = -largs -L/usr/X11R6/lib -lX11

MAKE = make

.SUFFIXES: .adb .ads .ali .o

all:
	mkdir -p objects
	for x in tabfx/*.adb ; do \
		$(ADAC) $(ADACFLAGS) -c $$x -o objects/$$(basename $$x .adb).o ; \
	done

clean:
	rm -fr objects

