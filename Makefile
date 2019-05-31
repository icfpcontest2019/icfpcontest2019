.PHONY: default all build

default: all

all:
	sbt assembly

build:
	sbt 'set test in assembly := {}' clean assembly
