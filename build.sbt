// Can't upgrade till this is resolve:
// https://github.com/ScalablyTyped/Converter/issues/565
scalaVersion := "3.3.1"

version := "0.2"

enablePlugins(ScalaJSPlugin)

enablePlugins(TzdbPlugin)

enablePlugins(ScalablyTypedConverterPlugin)
//enablePlugins(ScalaJSBundlerPlugin)
enablePlugins(WebScalaJSBundlerPlugin)

//stTypescriptVersion := "4.2.3"

pipelineStages in Assets := Seq(scalaJSPipeline)
//  .settings(
//    scalaJSProjects := Seq(client),
//  )
//  .enablePlugins(WebScalaJSBundlerPlugin)

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

githubTokenSource := TokenSource.Or(
  TokenSource.Environment("GITHUB_TOKEN"), // Injected during a github workflow for publishing
  TokenSource.GitConfig("github.token") // local token set in ~/.gitconfig
)

resolvers += "jitpack" at "https://jitpack.io"
resolvers += Resolver.url("typesafe", url("https://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
resolvers += Resolver.githubPackages("swoogles", "BulmaScala")
resolvers += Resolver.githubPackages("swoogles", "ScalaJsZioLibrary")

// This is only for quick dev turn-around
resolvers += "Sonatype" at "https://s01.oss.sonatype.org/content/repositories/public"



val zioVersion = "2.0.21"

scalacOptions ++= Seq("-Xmax-inlines", "150")

Compile / npmDependencies ++= Seq(
//  "@material-ui/core" -> "4.11.1",
//  "@material-ui/pickers" -> "3.2.10",
//  "@material/mwc-button" -> "0.18.0",
//  "@materiae-ui/pickers" -> "3.2.10",
  "smart-webcomponents" -> "16.0.1",
//  "react" -> "17.0.1"
)

/*
[error] npm WARN @material-ui/core@4.11.1 requires a peer of react@^16.8.0 but none is installed. You must install peer dependencies yourself.
[error] npm WARN @material-ui/core@4.11.1 requires a peer of react-dom@^16.8.0 but none is installed. You must install peer dependencies yourself.                                             [error] npm WARN @material-ui/pickers@3.2.10 requires a peer of react@^16.8.4 but none is installed. You must install peer dependencies yourself.
[error] npm WARN @material-ui/pickers@3.2.10 requires a peer of react-dom@^16.8.4 but none is installed. You must install peer dependencies yourself.
[error] npm WARN @material-ui/styles@4.11.2 requires a peer of react-dom@^16.8.0 || ^17.0.0 but none is installed. You must install peer dependencies yourself.                                [error] npm WARN @material-ui/system@4.11.2 requires a peer of react-dom@^16.8.0 || ^17.0.0 but none is installed. You must install peer dependencies yourself.
[error] npm WARN @material-ui/utils@4.11.2 requires a peer of react-dom@^16.8.0 || ^17.0.0 but none is installed. You must install peer dependencies yourself.
[error] npm WARN react-transition-group@4.4.1 requires a peer of react-dom@>=16.6.0 but none is installed. You must install peer dependencies yourself.

 */

libraryDependencies ++= Seq(
  "com.billding" %%% "bulmascala" % "0.2.22",
  "com.billding" %%% "scalajsziolibrary" % "0.0.16",
  "com.billdingsoftware" %%% "timepicker" % "0.3.15",
  "dev.zio" %%% "zio" % zioVersion,
  "dev.zio" %%% "zio-streams" % zioVersion,
  "dev.zio" %%% "zio-streams" % zioVersion,
  "dev.zio" %%% "zio-json" % "0.6.2",
  "com.lihaoyi" %%% "scalatags" % "0.12.0",
  "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
  "dev.zio" %%% "zio-test"     % zioVersion % "test",
  "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",
  "com.lihaoyi" %%% "pprint" % "0.8.1",
  "com.raquo" %%% "laminar" % "16.0.0",
  "com.raquo" %%% "airstream" % "16.0.0",
  "com.raquo" %%% "domtypes" % "17.1.0",
  "com.lihaoyi" %%% "upickle" % "3.1.3",
  "com.raquo" %%% "waypoint" % "7.0.0",   // Requires Airstream 0.12.0 & URL DSL 0.3.2
  "com.beachape" %%% "enumeratum" % "1.7.3"

//  "com.github.japgolly.scalacss" %%% "core" % "0.6.0",
//  "com.github.japgolly.scalacss" %%% "ext-scalatags" % "0.6.0",
)

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.8.1" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")


testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

lazy val cbBuild = taskKey[Unit]("Execute the shell script")

cbBuild := {
  (Compile/fastOptJS).value
//  (Compile/scalafmt).value
  import scala.sys.process._
//  "ls ./target/scala-2.13" !
  (Process("mkdir ./src/main/resources/compiledJavascript") #|| Process("mkdir ./src/main/resources/compiledJavascript/cb-bus") #||
    Process("cp ./target/scala-3.3.1/cb-bus-fastopt/main.js ./src/main/resources/compiledJavascript/cb-bus") #&&
    Process("cp ./target/scala-3.3.1/cb-bus-fastopt/main.js.map ./src/main/resources/compiledJavascript/cb-bus") #&&
//    Process("cp ./target/scala-2.13/cb-bus-fastopt/main.js.map ./src/main/resources/compiledJavascript/cb-bus") #&&
//    Process("cp ./target/scala-2.13/cb-bus-fastopt.js.map ./src/main/resources/compiledJavascript/") #&&
    Process("cp sw/target/scala-3.3.1/sw-opt.js ./src/main/resources/") #&&
    Process("cp sw/target/scala-3.3.1/sw-opt.js.map ./src/main/resources/")
//    Process("cp ./target/scala-2.13/cb-bus-jsdeps.js ./src/main/resources/compiledJavascript/")
    )!
}

lazy val cbPublish = taskKey[Unit]("Build the files for a real deploment")
cbPublish := {
  (Compile/fullOptJS).value
  (Compile/scalafmt).value
//  (sw/Compile/fullOptJS).value
  import scala.sys.process._
  //  "ls ./target/scala-2.13" !
  (Process("mkdir ./src/main/resources/compiledJavascript") #||
    Process("cp ./target/scala-3.3.1/cb-bus-opt/main.js ./src/main/resources/compiledJavascript/") #&&
    Process("cp ./target/scala-3.3.1/cb-bus-opt/main.js.map src/main/resources/compiledJavascript/") #&&
    Process("cp sw/target/scala-3.3.1/sw-opt.js src/main/resources/") #&&
    Process("cp sw/target/scala-3.3.1/sw-opt.js.map src/main/resources/"))!
}

zonesFilter := {(z: String) => z == "America/Denver" || z == "America/Mountain"}

//lazy val sw = (project in file("sw"))
//  .enablePlugins(ScalaJSPlugin)
//  .settings(
//    scalaVersion := "3.3.1",
//    scalaJSUseMainModuleInitializer := true,
//    libraryDependencies ++= Seq(
//      "org.scala-js" %%% "scalajs-dom" % "2.8.0"
//    )
//  )
