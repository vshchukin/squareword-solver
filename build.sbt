ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "org.squarewordsolver"

lazy val hello = (project in file("."))
  .settings(
    name := "squareword-solver",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test
  )
