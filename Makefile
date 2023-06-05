main:
	ghc --make -threaded -outputdir build -o solver Main.hs

clean:
	rm -rf build solver
