.PHONY: default all build

default: all

all: 
	rm -f ./icfpcontest2019.jar
	sbt clean fullOptJS assembly
	cp ./infra/target/scala-2.12/infra-assembly-1.0.0.jar ./icfpcontest2019.jar

clean:
	sbt clean

js:
	sbt fullOptJS

test:
	sbt test

zip: 
	rm -rf artifacts
	mkdir -p artifacts
	zip -j artifacts/part-1-initial.zip deliverables/contest/part-1/{*.desc,*.txt}
	zip -j artifacts/part-1-examples.zip deliverables/examples/part-1-examples/{*.desc,*.sol}
	zip -j artifacts/part-2-teleports.zip deliverables/contest/part-2/{*.desc,*.txt}
	zip -j artifacts/part-2-teleports-examples.zip deliverables/examples/part-2-examples/{*.desc,*.sol}
	zip -j artifacts/part-3-clones.zip deliverables/contest/part-3/{*.desc,*.txt}
	zip -j artifacts/part-3-clones-examples.zip deliverables/examples/part-3-examples/{*.desc,*.sol}
	zip -j artifacts/chain-puzzle-examples.zip deliverables/examples/chain-puzzle/{*.desc,*.cond}
	zip -j artifacts/purchasing-examples.zip deliverables/examples/purchasing-boosters-examples/{*.desc,*.sol,*.buy}
