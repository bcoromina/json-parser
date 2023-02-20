import Dependencies._

ThisBuild / scalaVersion := "2.13.9"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.bcoromina"
ThisBuild / organizationName := "bcoromina"

lazy val root = (project in file("."))
  .settings(
    name := "json-parser",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaTestFunSuite % Test,
  )
