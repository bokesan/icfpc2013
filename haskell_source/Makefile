.PHONY: clean profile

src = $(shell echo *.hs)

Main: $(src)
	ghc -Wall -O -rtsopts --make Main

profile:
	ghc -Wall -O -prof -caf-all -auto-all -rtsopts --make Main


clean:
	rm -f Main *.hi *.o *.prof *.aux *.hp *.ps

