vmc : CCFLAGS=-c
debug : CCFLAGS=-c -D_DEBUG

vmc debug: parser.o vheap.o token.o vthread.o vcodebase.o vinstrspec.o extfun.o unixapi.o cmdparse.o
	g++ -o vmc parser.o vheap.o token.o vthread.o vcodebase.o vinstrspec.o extfun.o unixapi.o cmdparse.o
parser.o: vmc.h parser.cpp
	g++ $(CCFLAGS) parser.cpp
vheap.o:  vmc.h vheap.cpp
	g++ $(CCFLAGS) vheap.cpp
token.o: vmc.h token.cpp
	g++ $(CCFLAGS) token.cpp
vthread.o: vmc.h vthread.cpp
	g++ $(CCFLAGS) vthread.cpp
vcodebase.o: vmc.h vcodebase.cpp
	g++ $(CCFLAGS) vcodebase.cpp
vinstrspec.o: vinstrspec.cpp vmc.h
	g++ $(CCFLAGS) vinstrspec.cpp
extfun.o: vmc.h extfun.cpp
	g++ $(CCFLAGS) extfun.cpp
unixapi.o: vmc.h unixapi.cpp
	g++ $(CCFLAGS) unixapi.cpp
cmdparse.o: cmdparse.h cmdparse.cpp
	g++ $(CCFLAGS) cmdparse.cpp
clean:
	rm -f *.o vmc
