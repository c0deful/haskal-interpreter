all:
	happy -gca ParHaskal.y
	alex -g LexHaskal.x
	ghc --make Main.hs -o interpreter
clean:
	-rm -f interpreter ParHaskal.hs LexHaskal.hs *.log *.hi *.o *.bak