all:
	ghc earTrainer.hs -threaded
	./cpMain
clean:
	rm earTrainer.hi earTrainer.o earTrainer Main Main.hi Main.o
