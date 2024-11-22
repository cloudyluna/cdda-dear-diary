all: 
	cabal build all

tests: 
	cabal test all

docs: 
	cabal haddock --haddock-hyperlinked-source \
				  --haddock-output-dir \
				docs cdda-dear-diary