#
# Makefile for CS 11 Haskell track, lab 4.
#

GHC    = ghc --make
HFLAGS = -W -O
PROGS  = Hello1 Hello2 Hello3 Hcat Hsort Huniq

compile: $(PROGS)

% : %.hs
	$(GHC) $(HFLAGS) $< $(LIBS) -o $@

check:
	haskell_style_check *.hs

clean:
	rm -f *.hi *.o $(PROGS)



