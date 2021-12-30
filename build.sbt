import Dependencies._

ThisBuild / scalaVersion := "2.13.7"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "nl.codecraftr"
ThisBuild / organizationName := "codecraftr"

lazy val fpInScala = (project in file("."))
  .settings(
    name := "Functional Programming in Scala",
    libraryDependencies += scalactic,
    libraryDependencies += scalaTest
  )
