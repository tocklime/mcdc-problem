.PHONY: testloop
testloop:
	stack exec -- ghcid -T":!make test"

.PHONY: test
test:
	stack test