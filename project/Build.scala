import sbt._
import Keys._

object BuildSettings {
  import Resolvers._

  val sv = "2.10.0"
  val buildOrganization = "zhaw"
  val buildVersion = "1.0.0-SNAPSHOT"
  val buildScalaVersion = sv

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq ("-deprecation", "-feature", "-language:postfixOps",
      "-language:higherKinds"),
    resolvers ++= Seq (scalatoolsRepo),
    exportJars := true,
    initialCommands in console := """
      import scalaz._, Scalaz._, effect.IO
      import zhaw.oe._
    """
  )
} 

object Resolvers {
 val scalatoolsRepo = "Scala-Tools Maven2 Repository Releases" at
   "http://scala-tools.org/repo-releases"
}

object Dependencies {
  import BuildSettings.sv
  val shapeless = "com.chuusai" %% "shapeless" % "1.2.3"
  val scala_reflect = "org.scala-lang" % "scala-reflect" % sv
  val scalaz_core = "org.scalaz" %% "scalaz-core" % "7.0.0-M7"
  val scalaz_effect = "org.scalaz" %% "scalaz-effect" % "7.0.0-M7"
  val scalaz_iteratee = "org.scalaz" %% "scalaz-iteratee" % "7.0.0-M7"
  val scalaz_scalacheck =
    "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.0-M7"
  val scalaz_scalacheckT = scalaz_scalacheck % "test"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0"
  val scalacheckT = scalacheck % "test"
}

object UtilBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  lazy val projectUtil = Project (
    "oe",
    file("."),
    settings = buildSettings ++ Seq (libraryDependencies ++=
      Seq(scalacheckT, scalaz_core, scalaz_effect,
        scalaz_scalacheck, scalaz_iteratee, scala_reflect, shapeless)) ++
    com.github.retronym.SbtOneJar.oneJarSettings
  )
}

// vim: set ts=2 sw=2 et:
