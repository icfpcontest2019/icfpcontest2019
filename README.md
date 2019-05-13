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
./checker -p <problemsPath> -s <colutionPath> -o <outputFile> 

```

For instance, from the root of the project you can run:

```
./checker team -p ./infra/src/test/resources/contest/toy/problems -s ./infra/src/test/resources/contest/toy/teams/hare/2019-05-11-12-03-00 -o ./results.csv -v true
```

Run `./checker team --help` for the reference.

### Scala programming and related frameworks to be used

* [Scala Programming Language](http://www.scala-lang.org/)
* [Akka HTTP](https://doc.akka.io/docs/akka-http/current/introduction.html)
* [Scala.js](http://www.lihaoyi.com/hands-on-scala-js/)
* [Scala Building Tool (SBT)](http://www.scala-sbt.org/)
* [ScalaCheck](https://scalacheck.org/)
* [ScalaTest](http://www.scalatest.org/)
* [SBT Revolver](https://github.com/spray/sbt-revolver)


