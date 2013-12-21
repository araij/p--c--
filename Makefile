EXECS   = Test
HSFLAGS = -Wall
SRCS    = $(wildcard *.hs)

all: $(EXECS)

.PHONY: $(EXECS)
$(EXECS):
	ghc $(HSFLAGS) --make $@

clean:
	$(RM) $(SRCS:.hs=.o) $(SRCS:.hs=.hi) $(EXECS)

