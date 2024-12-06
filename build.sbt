ThisBuild / organization := "se.johan1a"
ThisBuild / scalaVersion := "3.5.2"

lazy val root = (project in file(".")).
  settings(
    name := "advent-of-code-2024",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M10" % Test
  )

(Test / test) := (Test / test).dependsOn(Test / scalafmtCheck).value
