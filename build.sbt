name := "pawns"

version := "1.0"

scalaVersion := "2.12.1"
libraryDependencies += "org.scalatest"              %% "scalatest"      % "3.0.1" % "test"

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)