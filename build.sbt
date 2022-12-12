ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent2022",
    idePackagePrefix := Some("dev.wogan.advent"),
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.4.1",
      "org.typelevel" %% "cats-collections-core" % "0.9.5",
      "org.typelevel" %% "cats-time" % "0.5.1",
      "org.typelevel" %% "cats-parse" % "0.3.8",
      "co.fs2" %% "fs2-core" % "3.4.0",
      "co.fs2" %% "fs2-io" % "3.4.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
    )
  )
