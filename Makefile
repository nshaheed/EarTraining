all:
	ghc earTrainer.hs -threaded
	./earTrainer

y:
	ghc noteheadtest.hs -threaded
	./noteheadtest
clean:
	rm earTrainer.hi earTrainer.o earTrainer Main Main.hi Main.o
