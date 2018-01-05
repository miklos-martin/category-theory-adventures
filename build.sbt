import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "category",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Adventures in Category Theory",

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ywarn-dead-code",
      "-Xfatal-warnings",
      "-Xlint"
    ),

    addCompilerPlugin(kindProjector),

    libraryDependencies ++= Seq(
      scalaReflect % scalaVersion.value % Test,
      scalaCheck % Test
    )
  )
