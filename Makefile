all:
	happy -gca ParHaskal.y
	alex -g LexHaskal.x
	ghc --make Main.hs -o Interpreter
clean:
	-rm -f Interpreter ParHaskal.hs LexHaskal.hs *.log *.hi *.o *.bak