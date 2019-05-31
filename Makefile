.PHONY: default all build

default: all

all: clean
	sbt assembly
	cp ./target/scala-2.12/icfpcontest2019.jar .

clean:
	sbt clean
	rm -f ./icfpcontest2019.jar
