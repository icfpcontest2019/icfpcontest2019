# ICFP Programming Contest 2019

A collection of libraries and scripts for checking the tasks of ICFP Contest 2019.

## Build Requirements

* sbt (version >=1.2.8)
* JDK 1.8.

## Building and Testing

To compile the entiry project and run all tests, execute

```
sbt test
```

To assemble the executable JARs, run

```
make build
```

or

```
sbt assembly
```

## Running the checker

After having run `sbt assembly`, you can execute the checker in various modes. You can run 

```
./checker --help
``` 

for basic information. 

To grade an individual folder with solutions against a folder with tasks and recore the results into a file, run

```
./checker -p <problemsFolderPath> -s <solutionFolderPath> -o <outputFile> 
```

For instance, from the root of the project you can run:

```
./checker team -p ./infra/src/main/resources/contest/final -s ./infra/src/main/resources/contest/solutions -o ./results.csv -v true
```

Run `./checker team --help` for the reference.

### Running the solver

The solver is invoked as a routine of the checker as follows:

```
./checker solver <problemsPath> <solutionsPath>
```

For instance:

```
./checker solver ./infra/src/main/resources/contest/final ~/tmp/solutions
```

### Generating a Task Matrix

```
./checker matrix <task_file.desc> <task_file.mat>
```

Make sure to put the files with correct extensions (the converter doesn't check those)!

### Checking submissions for a given block

The format for block checking is as follows:

```
./checker block -p <current_task.mat> -c <lambda.chain> -b <blockNum> -s <submissionFolder> -o <path-to-scores.csv> -v <true>
```

For example, in the root of this project run:

```
./checker block -p ./infra/src/main/resources/blockchain/genesis/prob-000.mat -c ./infra/src/main/resources/blockchain/lambda.chain -b 1 -s ./infra/src/main/resources/blockchain/test/1/submissions -o ./infra/src/main/resources/blockchain/test/1 -v true
```

## Location of important files:

* `icfpcontest2019/infra/src/main/resources/blockchain` -- lambda-chain relate resources:
  * `lambda.chain` -- specification for future tasks (one peer line)
  * `genesis` -- a description, matrix (and a pretty bad solution) for the genesis block

* `icfpcontest2019/infra/src/main/resources/blockchain` -- main contest files  
  * `deliverables` -- three folders to be given to the contestants
  * `final` -- same tasks, but with matrices


## Compiling the JavaScript checker

First, run from terminal:

```
sbt fullOptJS
``` 

Then open the web-page `./graphics/src/main/resources/validate.html` which contains the testing snippet.

## Helper Scripts for Problem Generation and Checking

* `TaskBrowser` -- show tasks in the folder
* `RawRoomGenerator` -- generate random room within a given box
* `AddObstaclesToTasks` -- adding obstacles to rooms
* `AddBoostersToTasks` -- adding boosters to the tasks
* `BatchTaskMatrixConverter` -- converting tasks to matrices, can use it on the server on deliverables to produce matrices
* `CountriesToRectilinear` -- make rectilinear country shapes from the geodata
* `SimpleSolver` -- a dumb solver for the problem


### Scala programming and related frameworks to be used

* [Scala Programming Language](http://www.scala-lang.org/)
* [Akka HTTP](https://doc.akka.io/docs/akka-http/current/introduction.html)
* [Scala.js](http://www.lihaoyi.com/hands-on-scala-js/)
* [Scala Building Tool (SBT)](http://www.scala-sbt.org/)
* [ScalaCheck](https://scalacheck.org/)
* [ScalaTest](http://www.scalatest.org/)
* [SBT Revolver](https://github.com/spray/sbt-revolver)


