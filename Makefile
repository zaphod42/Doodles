clean:
	rm -f *.hi
	rm -f *.o
	rm -f structure

run:
	ghc --make structure.hs
	./structure
