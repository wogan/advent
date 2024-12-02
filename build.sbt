import kotlin.Keys.*

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "dev.wogan"

lazy val adventScala = (project in file("scala"))
  .settings(
    name := "advent-scala",
    scalaVersion := "3.6.1",
    scalacOptions += "-target:21",
    idePackagePrefix := Some("dev.wogan.advent.scala"),
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.7",
      "org.typelevel" %% "cats-collections-core" % "0.9.9",
      "org.typelevel" %% "cats-time" % "0.5.1",
      "org.typelevel" %% "cats-parse" % "1.0.0",
      "co.fs2" %% "fs2-core" % "3.11.0",
      "co.fs2" %% "fs2-io" % "3.11.0",
      "org.scalameta" %% "munit" % "1.0.2" % Test,
      "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test,
    )
  )

lazy val adventKotlin = (project in file("kotlin"))
  .enablePlugins(KotlinPlugin)
  .settings(
    name := "advent-kotlin",
    kotlinVersion := "2.1.0",
    kotlincJvmTarget := "21",
    autoScalaLibrary := false,
//    idePackagePrefix := Some("dev.wogan.advent.kotlin"),
    kotlinLib("stdlib"),
    libraryDependencies := Seq(
      "org.jetbrains.kotlinx" % "kotlinx-coroutines-core" % "1.9.0",
    ),
    Compile / unmanagedSourceDirectories := (Compile / kotlinSource).value :: Nil,
    Test / unmanagedSourceDirectories := (Test / kotlinSource).value :: Nil,
  )

lazy val adventJava = (project in file("java"))
  .settings(
    name := "advent-java",
    autoScalaLibrary := false,
    javacOptions ++= Seq("-source", "21", "-target", "21"),
    idePackagePrefix := Some("dev.wogan.advent.java"),
    Compile / unmanagedSourceDirectories := (Compile / javaSource).value :: Nil,
    Test / unmanagedSourceDirectories := (Test / javaSource).value :: Nil,
  )
