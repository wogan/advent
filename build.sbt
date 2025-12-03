import kotlin.Keys.*

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "dev.wogan"

lazy val adventScala = (project in file("scala"))
  .settings(
    name := "advent-scala",
    scalaVersion := "3.7.4",
    scalacOptions ++= Seq("-Xtarget:21", "-Xkind-projector"), // intellij wants the -Y version
    idePackagePrefix := Some("dev.wogan.advent.scala"),
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-effect" % "3.6.3",
      "org.typelevel" %% "cats-collections-core" % "0.9.10",
      "org.typelevel" %% "cats-time" % "0.6.0",
      "org.typelevel" %% "cats-parse" % "1.1.0",
      "co.fs2" %% "fs2-core" % "3.12.2",
      "co.fs2" %% "fs2-io" % "3.12.2",
      "org.scalameta" %% "munit" % "1.2.1" % Test,
      "org.typelevel" %% "munit-cats-effect" % "2.1.0" % Test,
      
    ),
    dependencyOverrides += "org.scala-lang" %% "scala3-library" % scalaVersion.value,
      dependencyOverrides += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
  )

lazy val adventKotlin = (project in file("kotlin"))
  .enablePlugins(KotlinPlugin)
  .settings(
    name := "advent-kotlin",
    kotlinVersion := "2.1.0",
    kotlincJvmTarget := "21",
    autoScalaLibrary := false,
    kotlinLib("stdlib"),
    kotlinRuntimeProvided := false,
    libraryDependencies := Seq(
      "org.jetbrains.kotlinx" % "kotlinx-coroutines-core" % "1.10.2",
      "io.arrow-kt" % "arrow-core" % "2.2.0",
      "io.arrow-kt" % "arrow-fx-coroutines" % "2.2.0"
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
