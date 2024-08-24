ThisBuild / scalaVersion := "3.3.3"

lazy val root = project
  .in(file("."))
  .aggregate(simplyTyped, solver)

lazy val solver = project
  .in(file("modules/solver"))
  .settings(
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "tools.aqua"     % "z3-turnkey"  % "4.13.0"
    )
  )

// Chapter 3
lazy val simplyTyped = project
  .in(file("modules/simply-typed"))
  .dependsOn(solver)
  .settings(
    libraryDependencies := Seq(
      "org.typelevel"       %% "cats-effect" % "3.5.4",
      "org.typelevel"       %% "cats-parse"  % "0.3.9",
      "com.disneystreaming" %% "weaver-cats" % "0.8.4" % Test
    )
  )
