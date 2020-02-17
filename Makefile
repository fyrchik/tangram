.PHONY: build run tests

build:
	mmc --make --use-subdirs tangram

run: build
	./tangram

tests:
	@cd tests && make tests
