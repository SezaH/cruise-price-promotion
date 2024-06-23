ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "TSTHomeAssignment"
  )

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.20.7" % Test
)
