ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2022"
  )

libraryDependencies += "org.scalaj" % "scalaj-http_2.13" % "2.4.2"