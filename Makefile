EXECS   = Test
HSFLAGS = -Wall
SRCS    = $(wildcard *.hs)

all: $(EXECS)

$(EXECS): %: %.o
	ghc $(HSFLAGS) --make $@

clean:
	$(RM) $(SRCS:.hs=.o) $(SRCS:.hs=.hi)

depend:
	ghc $(HSFLAGS) -M $(EXECS)

%.o: %.hs
	$(RM) $@
%.hi: %.hs %.o
	$(RM) $@
	ghc $(HSFLAGS) -c $<

# DO NOT DELETE: Beginning of Haskell dependencies
ParserUtil.o : ParserUtil.hs
Dot.o : Dot.hs
Upc.o : Upc.hs
Upc.o : ParserUtil.hi
UpcToDot.o : UpcToDot.hs
UpcToDot.o : Dot.hi
UpcToDot.o : Upc.hi
Test.o : Test.hs
Test.o : UpcToDot.hi
Test.o : Dot.hi
Test.o : Upc.hi
# DO NOT DELETE: End of Haskell dependencies
