EXECS   = p-c
HSFLAGS = -Wall -O
SRCS    = $(wildcard *.hs)

all: $(EXECS)

.PHONY: $(EXECS)
p-c:
	ghc $(HSFLAGS) --make Main -o $@

clean:
	$(RM) $(SRCS:.hs=.o) $(SRCS:.hs=.hi) $(EXECS)

