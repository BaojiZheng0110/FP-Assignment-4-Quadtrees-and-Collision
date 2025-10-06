val scala3Version = "3.7.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Scala 3 QuadTree Project",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.scalameta" %% "munit-scalacheck" % "1.0.0" % Test,
      // (optional, but harmless) add ScalaCheck explicitly:
      "org.scalacheck" %% "scalacheck" % "1.18.0" % Test
    )
  )
