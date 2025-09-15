ThisBuild / scalaVersion := "3.3.6"

version := "0.2"

lazy val root = (project in file("."))
  .aggregate(common, frontend, sw)

lazy val common = (project in file("common"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % "3.1.3",
      "dev.zio" %%% "zio-json" % "0.6.2",
    )
  )

lazy val frontend = (project in file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(WebScalaJSBundlerPlugin)
  .dependsOn(common)
  .settings(
    pipelineStages in Assets := Seq(scalaJSPipeline),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    scalacOptions ++= Seq("-Xmax-inlines", "150"),
    Compile / fullOptJS := (Compile / fullOptJS).dependsOn(Compile / scalafmt).value,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % "0.6.2",
      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
      "dev.zio" %%% "zio-test"     % zioVersion % "test",
      "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",
      "com.lihaoyi" %%% "pprint" % "0.8.1",
      "com.raquo" %%% "laminar" % "16.0.0",
      "com.raquo" %%% "airstream" % "16.0.0",
      "com.raquo" %%% "domtypes" % "17.1.0",
      "com.lihaoyi" %%% "upickle" % "3.1.3",
      "com.raquo" %%% "waypoint" % "7.0.0",   // Requires Airstream 0.12.0 & URL DSL 0.3.2
      "io.github.kitlangton" %%% "animus" % "0.6.5",
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    ),
    scalaJSUseMainModuleInitializer := true,
    Compile / fastOptJS / artifactPath := 
      baseDirectory.value / "src" / "main" / "resources" / "compiledJavascript" / "main.js",
    Compile / fullOptJS / artifactPath := 
      baseDirectory.value / "src" / "main" / "resources" / "compiledJavascript" / "main.js"
  )

lazy val sw = (project in file("sw"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(common)
  .settings(
    Compile / fullOptJS := (Compile / fullOptJS).dependsOn(Compile / scalafmt).value,
    Compile / fastOptJS / artifactPath := 
      baseDirectory.value.getParentFile / "frontend" / "src" / "main" / "resources" / "sw.js",
    Compile / fullOptJS / artifactPath := 
      baseDirectory.value.getParentFile / "frontend" / "src" / "main" / "resources" / "sw.js",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % "0.6.2",
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
    )
  )

val zioVersion = "2.0.21"

Global / onChangedBuildSource := ReloadOnSourceChanges