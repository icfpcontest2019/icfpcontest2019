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

## Helper Scripts for Problem Generation and Checking

* `TaskBrowser` -- show tasks in the folder
* `RawRoomGenerator` -- generate random room within a given box
* `AddObstaclesToTasks` -- adding obstacles to rooms
* `AddBoostersToTasks` -- adding boosters to the tasks
* `BatchTaskMatrixConverter` -- converting tasks to matrices
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


