%: %.hs
	mkdir -p .objects
	ghc $< --make -odir .objects -hidir .objects -o $@
