name := "ninety-nine-problem"

version := "1.0"

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-deprecation"
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)
