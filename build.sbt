import sbt.Keys._

scalaVersion in ThisBuild := "2.11.8"

val project_name = "scalajs-console-game"
val project_version = "1.0.0-SNAPSHOT"

val artifactPrefix = "target/scala-2.11/" + project_name + "-" + project_version

scalacOptions ++= Seq("-feature", "-deprecation")

lazy val root = (project in file("."))
  .settings(
    organization := "org.enricobn",
    name := project_name,
    version := project_version,
    artifactPath in (Compile, fullOptJS) := baseDirectory.value / (artifactPrefix + ".min.js"),
    artifactPath in (Compile, packageJSDependencies) := baseDirectory.value / (artifactPrefix + "-jsdeps.js"),
    artifactPath in (Compile, packageMinifiedJSDependencies) := baseDirectory.value / (artifactPrefix + "-jsdeps.min.js"),
//    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    libraryDependencies += "org.enricobn" %%% "scalajs-shell" % "1.0.0-SNAPSHOT" changing(),
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.3",

    //  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % "2.11.8"),
    // TEST
    //libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % Test,
    libraryDependencies += "org.scalamock" %%% "scalamock" % "4.1.0" % Test,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % Test,
    //testFrameworks += new TestFramework("utest.runner.Framework"),
    //scalaJSUseMainModuleInitializer := true,
    testOptions in Test += Tests.Argument("-oF")
  )
  .enablePlugins(ScalaJSPlugin)
    
