import sbt._
import Keys._
import sbt.Defaults._

object PlaygroundBuild extends Build {

  val buildScalaVersion = "2.10.1"

  val scalaLibrary = "org.scala-lang" % "scala-library" % buildScalaVersion
  val scalaReflect = "org.scala-lang" % "scala-reflect" % buildScalaVersion

  val scalatest = "org.scalatest" %% "scalatest" % "2.0.M5b"

  lazy val standardDependencies = Seq(
    libraryDependencies ++= Seq(
      scalaLibrary,
      scalaReflect,
      scalatest
    )
  )

  lazy val standardSettings = defaultSettings ++ standardDependencies ++ Seq(
    scalaVersion := buildScalaVersion,
    testOptions in Test ++= Seq(
      // D: show durations
      // F: show full stack traces
      Tests.Argument(TestFrameworks.ScalaTest, "-oD")
    ),
    javacOptions += "-g",
    // javaOptions in Test += "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5006",
    javaOptions in Test += "-XX:+HeapDumpOnOutOfMemoryError",
    javaOptions in Test += "-XX:HeapDumpPath=/tmp/broke.dump",
    javaOptions in Test += "-Xmx512m",
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-feature",
      "-language:postfixOps", // enable implicit conversions globally
      "-language:implicitConversions", // enable postfix notation globally
      "-language:reflectiveCalls", // enable reflective access of structural type members globally
      "-language:existentials",
      "-encoding",
      "utf8"
    ),
    fork in Test := true
  )

  lazy val root = Project(
    "master", file("."),
    settings = Seq(
      scalaVersion := buildScalaVersion
    ) ++ standardSettings
  )

}
