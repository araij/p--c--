EXEC     = Test
HSFLAGS = -Wall
SRCS     = $(wildcard *.hs)

all: $(EXEC)

clean:
	$(RM) $(SRCS:.hs=.o) $(SRCS:.hs=.hi)

depend:
	ghc $(HSFLAGS) -M $(EXEC)

%.o: %.hs
	$(RM) $@
	ghc $(HSFLAGS) -c $<
%.hi: %.hs %.o
	$(RM) $@
	ghc $(HSFLAGS) -c $<
%: %.o
	ghc $(HSFLAGS) --make $@

# DO NOT DELETE: Beginning of Haskell dependencies
Dot.o : Dot.hs
Upc.o : Upc.hs
UpcToDot.o : UpcToDot.hs
UpcToDot.o : Dot.hi
UpcToDot.o : Upc.hi
Test.o : Test.hs
Test.o : UpcToDot.hi
Test.o : Dot.hi
Test.o : Upc.hi
# DO NOT DELETE: End of Haskell dependencies
