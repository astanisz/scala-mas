name := "examples"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "pl.edu.agh.scalamas" %% "emas" % "0.1"

mainClass in assembly := Some("pl.edu.agh.scalamas.examples.EmasApp")