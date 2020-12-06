import sbt._
import sbt.Keys._


object BuildSettings {
  val buildOrganization = "org.squarewordsolver"
  val buildVersion = "0.0.11"
  val buildScalaVersion = "2.13.4"
  val projectName = "squareword-solver"

  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object ShellPrompt {

  object devnull extends ProcessLogger {
    def info(s: => String) {}

    def error(s: => String) {}

    def buffer[T](f: => T): T = f
  }
}

object Resolvers {
  val sbtAssemblyRepo = Resolver.url("artifactory",
    url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)
}

object Dependencies {
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
  val junit = "junit" % "junit" % "4.11" % "test"
}

object SbtAssembly {
  import sbtassembly.Plugin._
  import sbtassembly.Plugin.AssemblyKeys._
  import BuildSettings._

  val asmbSettings = assemblySettings ++ Seq(jarName in assembly:=s"$projectName-$buildVersion.jar",
    test in assembly:={})
}

object MainBuild extends Build {

  import Resolvers._
  import Dependencies._
  import BuildSettings._
  import SbtAssembly._

  lazy val main = Project(projectName, file("."))
    .settings(buildSettings: _*)
    .settings(asmbSettings: _*)
    .settings(resolvers ++= Seq(sbtAssemblyRepo))
    .settings(libraryDependencies ++= Seq(scalaTest, junit))
    .settings(scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-target:jvm-1.7"))
    .settings(javacOptions ++= Seq("-source", "1.7", "-target", "1.7"))
}