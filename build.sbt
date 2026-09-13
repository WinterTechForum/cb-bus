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
      "dev.zio" %%% "zio-test" % zioVersion % Test,
      "dev.zio" %%% "zio-test-sbt" % zioVersion % Test,
    ),
    Test / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
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
    // Version the complete offline shell from its source inputs. Each release
    // installs in a separate cache, so a failed download cannot damage the
    // working release. No generated bundles or source maps enter this hash.
    Compile / sourceGenerators += Def.task {
      val repo = baseDirectory.value.getParentFile
      val resources = repo / "frontend" / "src" / "main" / "resources"
      val assets = Seq(resources / "index.html", resources / "manifest.webmanifest",
        resources / "favicon.ico", resources / "javascript" / "long-press-event.min.js") ++
        (resources / "styling" ** "*.css").get ++
        (resources / "glyphicons" ** "*.svg").get ++
        (resources / "images" ** "*.png").get
      val sources = Seq("common", "frontend", "sw").flatMap { module =>
        (repo / module / "src" / "main" / "scala" ** "*.scala").get
      }
      val inputs = (assets ++ sources ++ Seq(repo / "build.sbt") ++ (repo / "project" * "*.sbt").get).sortBy(_.getPath)
      val digest = java.security.MessageDigest.getInstance("SHA-256")
      inputs.foreach { f =>
        digest.update(IO.relativize(repo, f).get.getBytes("UTF-8"))
        digest.update(IO.readBytes(f))
      }
      val version = digest.digest().map(b => f"${b & 0xff}%02x").mkString.take(20)
      val paths = (Seq("/compiledJavascript/main.js") ++ assets.map(f => "/" + IO.relativize(resources, f).get)).sorted
      val quoted = paths.map(p => "\"" + p + "\"").mkString(",\n")
      val out = (Compile / sourceManaged).value / "todo" / "OfflineManifest.scala"
      IO.write(out, s"""package todo
object OfflineManifest {
  val cacheName = "cb-bus-shell-$version"
  val assets: List[String] = List($quoted)
}
""")
      Seq(out)
    }.taskValue,
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

// JVM-only module: end-to-end browser tests that drive the built app via
// Playwright for Java. All test logic is Scala. Kept out of the root aggregate
// so `sbt test` stays fast; run these with `sbt e2e/test`.
lazy val e2e = (project in file("e2e"))
  .settings(
    scalaVersion := "3.3.6",
    libraryDependencies ++= Seq(
      "com.microsoft.playwright" % "playwright" % "1.49.0",
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
    ),
    Test / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    Test / fork := true,
    // Absolute path to the app's static resources, resolved regardless of the
    // forked test's working directory.
    Test / javaOptions +=
      "-Dapp.resources=" +
        ((frontend / baseDirectory).value / "src" / "main" / "resources").getAbsolutePath,
  )

val zioVersion = "2.0.21"

Global / onChangedBuildSource := ReloadOnSourceChanges
