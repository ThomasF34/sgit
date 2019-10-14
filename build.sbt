import Dependencies._
import sbtassembly.AssemblyPlugin.defaultUniversalScript

ThisBuild / scalaVersion := "2.13.0"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "fr.igpolytech"
ThisBuild / organizationName := "igpolytech"

lazy val root = (project in file("."))
  .settings(
    name := "sgit",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
  )

assemblyOption in assembly := (assemblyOption in assembly).value
  .copy(prependShellScript = Some(defaultUniversalScript(shebang = false)))

assemblyJarName in assembly := s"${name.value}"

parallelExecution in Test := false

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
