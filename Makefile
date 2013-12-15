EXEC     = Test
HS_FLAGS = -Wall
SRCS     = $(wildcard *.hs)

all: $(EXEC)

clean:
	$(RM) $(SRCS:.hs=.o)


depend:
	ghc $(HS_FLAGS) -M $(EXEC)

%.hi: %.hs
	$(RM) $@
	ghc $(HS_FLAGS) -c -o $@ $<

%.o: %.hs
	$(RM) $@
	ghc $(HS_FLAGS) -c -o $@ $<

%: %.o
	ghc $(HS_FLAGS) --make -o $@ $(basename $<)

# DO NOT DELETE: Beginning of Haskell dependencies
Upc.o : Upc.hs
Dot.o : Dot.hs
Dot.o : Upc.hi
UpcToDot.o : UpcToDot.hs
UpcToDot.o : Dot.hi
UpcToDot.o : Upc.hi
Test.o : Test.hs
Test.o : UpcToDot.hi
Test.o : Dot.hi
Test.o : Upc.hi
# DO NOT DELETE: End of Haskell dependencies
