ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent2022",
    idePackagePrefix := Some("dev.wogan.advent"),
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.6",
      "org.typelevel" %% "cats-collections-core" % "0.9.9",
      "org.typelevel" %% "cats-time" % "0.5.1",
      "org.typelevel" %% "cats-parse" % "1.0.0",
      "co.fs2" %% "fs2-core" % "3.11.0",
      "co.fs2" %% "fs2-io" % "3.11.0",
      "org.scalameta" %% "munit" % "1.0.2" % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
    )
  )
