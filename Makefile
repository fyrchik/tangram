.PHONY: build run tests

build:
	mmc --make --use-subdirs command

run: build
	./command

tests:
	@cd tests && make tests
