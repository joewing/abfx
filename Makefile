
PROJECTS = tabfx calc

all:
	for x in $(PROJECTS) ; do (cd $$x && make) ; done;

clean:
	for x in $(PROJECTS) ; do (cd $$x && make clean) ; done;

