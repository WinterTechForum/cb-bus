// Can't upgrade till this is resolve:
// https://github.com/ScalablyTyped/Converter/issues/565
ThisBuild / scalaVersion := "3.3.6"

version := "0.2"

enablePlugins(ScalaJSPlugin)

// enablePlugins(TzdbPlugin)

enablePlugins(WebScalaJSBundlerPlugin)

pipelineStages in Assets := Seq(scalaJSPipeline)

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

resolvers += Resolver.url("typesafe", url("https://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)

// This is only for quick dev turn-around
resolvers += "Sonatype" at "https://s01.oss.sonatype.org/content/repositories/public"



val zioVersion = "2.0.21"

scalacOptions ++= Seq("-Xmax-inlines", "150")

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
)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

lazy val cbBuild = taskKey[Unit]("Execute the shell script")

Compile / fastOptJS / artifactPath := 
  baseDirectory.value / "src" / "main" / "resources" / "compiledJavascript" / "main.js"
Compile / fullOptJS / artifactPath := 
  baseDirectory.value / "src" / "main" / "resources" / "compiledJavascript" / "main.js"



cbBuild := {
  (Compile/fastOptJS).value
  import scala.sys.process._
  (Process("mkdir ./src/main/resources/compiledJavascript") #|| Process("mkdir ./src/main/resources/compiledJavascript/cb-bus") #||
    // Process("cp ./target/scala-3.3.1/cb-bus-fastopt/main.js ./src/main/resources/compiledJavascript/cb-bus") #&&
    // Process("cp ./target/scala-3.3.1/cb-bus-fastopt/main.js.map ./src/main/resources/compiledJavascript/cb-bus") #&&
    Process("cp sw/target/scala-3.3.1/sw-opt.js ./src/main/resources/") #&&
    Process("cp sw/target/scala-3.3.1/sw-opt.js.map ./src/main/resources/")
    )!
}

lazy val cbPublish = taskKey[Unit]("Build the files for a real deploment")
cbPublish := {
  (sw/Compile/fullOptJS).value
  (Compile/fullOptJS).value
  (Compile/scalafmt).value
}

// zonesFilter := {(z: String) => z == "America/Denver" || z == "America/Mountain"}

lazy val sw = (project in file("sw"))
 .enablePlugins(ScalaJSPlugin)
 .settings(
    Compile / fastOptJS / artifactPath := 
      baseDirectory.value.getParentFile / "src" / "main" / "resources" / "sw-opt.js",
    Compile / fullOptJS / artifactPath := 
      baseDirectory.value.getParentFile / "src" / "main" / "resources" / "sw-opt.js",
   scalaJSUseMainModuleInitializer := true,
   libraryDependencies ++= Seq(
     "org.scala-js" %%% "scalajs-dom" % "2.8.0"
   )
 )

Global / onChangedBuildSource := ReloadOnSourceChanges