.PHONY: testloop
testloop:
	stack exec -- ghcid -T":!make test"

.PHONY: test
test:
	stack test

.PHONY: docs
docs:
	stack exec -- haddock -h src/*.hs --hyperlinked-source --odir=docs
