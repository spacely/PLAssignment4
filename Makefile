#
#
#
all:
	ghc -o while WhileInterpreter.hs

clean:
	rm *.o *.hi
