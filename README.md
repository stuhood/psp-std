psp.std - a non-standard library
================================

[![Build Status](https://travis-ci.org/paulp/psp-std.svg?branch=master)](https://travis-ci.org/paulp/psp-std) [![Code Coverage](http://codecov.io/github/paulp/psp-std/coverage.svg?branch=master)](http://codecov.io/github/paulp/psp-std?branch=master) [![Join the chat at https://gitter.im/paulp/psp-std](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/paulp/psp-std?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

### Background

The scala standard library is deficient in many ways. This library is an attempt to rebuild it with some attention given to consistency, performance, and correctness. See [views](doc/views.md) for some details. See [overview](doc/overview.md) for the project layout.

### Usage

Suggested contents for `build.sbt`:

```scala
                      name :=  "scratch"
                 resolvers +=  "bintray/paulp" at "https://dl.bintray.com/paulp/maven"
              scalaVersion :=  "2.11.7"
initialCommands in console :=  "import psp._, std._, api._, StdEq._"
       libraryDependencies +=  "org.improving" %% "psp-std" % "0.5.6"
```

Then `sbt console` and you can look around.
```scala
Welcome to the Ammonite Repl 0.4.8
(Scala 2.11.7 Java 1.8.0_66)
@ vec(1, 2, 3)
res0: Vec[Int] = [ 1, 2, 3 ]

@ vec(1, 2, 3).m mapOnto (_ * 10)
res1: api.ExMap[Int, Int] = 1 -> 10
2 -> 20
3 -> 30
```

### Requirements

scala 2.11, java 8, sbt 0.13.7+
