all:
	ghc earTrainer.hs -threaded
	./earTrainer
clean:
	rm earTrainer.hi earTrainer.o earTrainer Main Main.hi Main.o
