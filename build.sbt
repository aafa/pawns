
name := "pawns"

version := "1.0"

scalaVersion := "2.11.11"
libraryDependencies += "org.scalatest"              %% "scalatest"      % "3.0.1" % "test"
libraryDependencies += "org.scala-js"               %%% "scalajs-dom"   % "0.9.0"
libraryDependencies += "com.lihaoyi"                %%% "scalatags"     % "0.6.5"
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

enablePlugins(ScalaJSPlugin, WorkbenchPlugin)