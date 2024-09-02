ThisBuild / scalaVersion := "3.3.3"

lazy val root = project
  .in(file("."))
  .aggregate(
    languages,
    parser,
    solver,
    simplyTyped
  )

// --------------
// Chapters
// --------------

// Chapter 3
lazy val simplyTyped = project
  .in(file("modules/simply-typed"))
  .dependsOn(languages, parser, solver)
  .settings(
    libraryDependencies := Seq(
      "org.typelevel"       %% "cats-effect" % "3.5.4",
      "com.disneystreaming" %% "weaver-cats" % "0.8.4" % Test
    )
  )

// --------------
// Common modules
// --------------

lazy val solver = project
  .in(file("modules/solver"))
  .dependsOn(languages, parser)
  .settings(
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "tools.aqua"     % "z3-turnkey"  % "4.13.0"
    )
  )

lazy val parser = project
  .in(file("modules/parser"))
  .settings(
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9"
    )
  )

lazy val languages = project
  .in(file("modules/languages"))
