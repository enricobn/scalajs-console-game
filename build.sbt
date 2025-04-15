import sbt.Keys.*

ThisBuild / scalaVersion := "3.6.4"

val project_name = "scalajs-console-game"
val project_version = "1.0.0-SNAPSHOT"

scalacOptions ++= Seq("-feature", "-deprecation")

lazy val root = (project in file("."))
  .settings(
    organization := "org.enricobn",
    name := project_name,
    version := project_version,
    libraryDependencies += "org.enricobn" %%% "scalajs-shell" % "1.0.0-SNAPSHOT" changing(),
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.1.0",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    libraryDependencies += "org.scalamock" %%% "scalamock" % "7.3.0" % Test
  )
  .enablePlugins(ScalaJSPlugin)

scalacOptions ++= Seq(
  "-deprecation"
)

