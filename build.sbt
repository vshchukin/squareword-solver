ThisBuild / scalaVersion := "2.13.4"
ThisBuild / organization := "org.squarewordsolver"

lazy val hello = (project in file("."))
  .settings(
    name := "squareword-solver",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
  )
