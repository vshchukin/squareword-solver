ThisBuild / scalaVersion := "3.1.2"
ThisBuild / organization := "org.squarewordsolver"

lazy val hello = (project in file("."))
  .settings(
    name := "squareword-solver",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
  )
