.PHONY: default all build

default: all

all: 
	git clean -dfXq
	sbt clean fullOptJS assembly
	cp ./infra/target/scala-2.12/infra-assembly-1.0.0.jar ./icfpcontest2019.jar

clean:
	sbt clean

js:
	sbt fullOptJS
	
test:
	sbt test
