.PHONY: build run

build:
	mmc --make --use-subdirs tangram

run: build
	./tangram
