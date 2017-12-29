import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "category",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Adventures in Category Theory",
    libraryDependencies ++= Seq(
      scalaCheck % Test
    )
  )
