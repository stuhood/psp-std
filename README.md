psp.std - a non-standard standard library
=========================================

[![Join the chat at https://gitter.im/paulp/psp-std](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/paulp/psp-std?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Build Status](https://travis-ci.org/paulp/psp-std.svg?branch=master)](https://travis-ci.org/paulp/psp-std)

Background
----------

The scala standard library is deficient in many ways. This library is an
attempt to rebuild it with some attention given to consistency, performance,
and correctness. See [views](views.md) for some details.

Usage
-----

Suggested contents for ```build.sbt``` - note this syntax requires sbt 0.13.7, the "blank lines are optional" release.

```scala
                      name :=  "scratch"
                 resolvers +=  "bintray/paulp" at "https://dl.bintray.com/paulp/maven"
              scalaVersion :=  "2.11.4"
initialCommands in console :=  "import psp._, std._, api._, StdEq._"
       libraryDependencies +=  "org.improving" %% "psp-std" % "0.5.4"
```

Then ```sbt console``` and you can look around.
```
scala> Array(1, 2) === Array(1, 2)
res0: Boolean = true
```

Requirements
------------

scala 2.10+, java 7+.
