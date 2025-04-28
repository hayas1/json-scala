val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Json Scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      "org.typelevel" %% "cats-core" % "2.13.0"
    )
  )
