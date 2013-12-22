EXECS   = p-c Test
HSFLAGS = -Wall
SRCS    = $(wildcard *.hs)

all: $(EXECS)

.PHONY: $(EXECS)
p-c:
	ghc $(HSFLAGS) --make Main -o $@
Test:
	ghc $(HSFLAGS) --make $@

clean:
	$(RM) $(SRCS:.hs=.o) $(SRCS:.hs=.hi) $(EXECS)

