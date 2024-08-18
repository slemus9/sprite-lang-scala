ThisBuild / scalaVersion := "3.3.3"

lazy val root = project
  .in(file("."))
  .aggregate(simplyTyped)

// Chapter 1
lazy val simplyTyped = project
  .in(file("modules/simply-typed"))
  .settings(
    libraryDependencies := Seq(
      "org.typelevel"       %% "cats-effect" % "3.5.4",
      "org.typelevel"       %% "cats-parse"  % "0.3.9",
      "com.disneystreaming" %% "weaver-cats" % "0.8.4" % Test
    )
  )
