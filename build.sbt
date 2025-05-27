ThisBuild / scalaVersion := "3.3.6"

version := "0.2"

lazy val root = (project in file("."))
  .aggregate(frontend, sw)

lazy val frontend = (project in file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(WebScalaJSBundlerPlugin)
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
    ),
    scalaJSUseMainModuleInitializer := true,
    Compile / fastOptJS / artifactPath := 
      baseDirectory.value / "src" / "main" / "resources" / "compiledJavascript" / "main.js",
    Compile / fullOptJS / artifactPath := 
      baseDirectory.value / "src" / "main" / "resources" / "compiledJavascript" / "main.js"
  )

lazy val sw = (project in file("sw"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Compile / fullOptJS := (Compile / fullOptJS).dependsOn(Compile / scalafmt).value,
    Compile / fastOptJS / artifactPath := 
      baseDirectory.value.getParentFile / "frontend" / "src" / "main" / "resources" / "sw-opt.js",
    Compile / fullOptJS / artifactPath := 
      baseDirectory.value.getParentFile / "frontend" / "src" / "main" / "resources" / "sw-opt.js",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0"
    )
  )

val zioVersion = "2.0.21"

Global / onChangedBuildSource := ReloadOnSourceChanges