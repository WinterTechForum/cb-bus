scalaVersion := "2.13.1"
version := "0.2"

enablePlugins(ScalaJSPlugin)

enablePlugins(TzdbPlugin)

enablePlugins(ScalablyTypedConverterPlugin)
//enablePlugins(ScalaJSBundlerPlugin)
enablePlugins(WebScalaJSBundlerPlugin)

pipelineStages in Assets := Seq(scalaJSPipeline)
//  .settings(
//    scalaJSProjects := Seq(client),
//  )
//  .enablePlugins(WebScalaJSBundlerPlugin)

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

resolvers += "jitpack" at "https://jitpack.io"
resolvers += Resolver.url("typesafe", url("https://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
resolvers += Resolver.githubPackages("swoogles", "BulmaScala")

// This is only for quick dev turn-around
resolvers += "Sonatype" at "https://s01.oss.sonatype.org/content/repositories/public"


val zioVersion = "1.0.0-RC21-2"

Compile / npmDependencies ++= Seq(
//  "react-router-dom" -> "5.1.2",
//  "@types/react-router-dom" -> "5.1.2",
  "@material-ui/core" -> "4.11.1",
  "@material-ui/pickers" -> "3.2.10",
//  "@material-ui/mwc-button" -> "0.20.0",
  "@material/mwc-button" -> "0.18.0",
  "@material-ui/pickers" -> "3.2.10",
  "smart-webcomponents" -> "9.0.0",
//  "timepicker-ui" -> "1.2.0",
//  "@types/timepicker-ui" -> "1.2.0",
  "react" -> "17.0.1"
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
  "com.billding" %%% "bulmalibrary" % "0.2.19",
  "com.billding" %%% "scalajsziolibrary" % "0.0.13",
  "com.billdingsoftware" %%% "timepicker" % "0.1.5",
  "dev.zio" %%% "zio" % zioVersion,
  "dev.zio" %%% "zio-streams" % zioVersion,
  "com.lihaoyi" %%% "scalatags" % "0.8.6",
  "io.github.cquiroz" %%% "scala-java-time" % "2.0.0",
  "dev.zio" %%% "zio-test"     % zioVersion % "test",
  "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",
  "com.lihaoyi" %%% "pprint" % "0.5.9",
  "com.raquo" %%% "laminar" % "0.12.1",
  "com.raquo" %%% "airstream" % "0.12.0",
  "com.lihaoyi" %%% "upickle" % "1.3.0",
  "com.raquo" %%% "waypoint" % "0.3.0",   // Requires Airstream 0.12.0 & URL DSL 0.3.2
  "com.beachape" %%% "enumeratum" % "1.6.1"

//  "com.github.japgolly.scalacss" %%% "core" % "0.6.0",
//  "com.github.japgolly.scalacss" %%% "ext-scalatags" % "0.6.0",
)

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.5" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")


testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

lazy val cbBuild = taskKey[Unit]("Execute the shell script")

cbBuild := {
  (Compile/fastOptJS).value
  (Compile/scalafmt).value
  import scala.sys.process._
//  "ls ./target/scala-2.13" !
  (Process("mkdir ./src/main/resources/compiledJavascript") #|| Process("mkdir ./src/main/resources/compiledJavascript/busriderapp") #||
    Process("cp ./target/scala-2.13/busriderapp-fastopt/main.js ./src/main/resources/compiledJavascript/busriderapp") #&&
    Process("cp ./target/scala-2.13/busriderapp-fastopt/main.js.map ./src/main/resources/compiledJavascript/busriderapp") #&&
//    Process("cp ./target/scala-2.13/busriderapp-fastopt.js.map ./src/main/resources/compiledJavascript/") #&&
    Process("cp sw/target/scala-2.12/sw-opt.js ./src/main/resources/") #&&
    Process("cp sw/target/scala-2.12/sw-opt.js.map ./src/main/resources/") #&&
    Process("cp ./target/scala-2.13/busriderapp-jsdeps.js ./src/main/resources/compiledJavascript/"))!
}

lazy val cbPublish = taskKey[Unit]("Build the files for a real deploment")
cbPublish := {
  (Compile/fullOptJS).value
  (Compile/scalafmt).value
  (sw/Compile/fullOptJS).value
  import scala.sys.process._
  //  "ls ./target/scala-2.13" !
  (Process("mkdir ./src/main/resources/compiledJavascript") ###
    Process("cp ./target/scala-2.13/busriderapp-opt/main.js ./src/main/resources/compiledJavascript/busriderapp-opt/") #&&
    Process("cp ./target/scala-2.13/busriderapp-opt.js.map src/main/resources/compiledJavascript/") ###
    Process("cp sw/target/scala-2.12/sw-opt.js src/main/resources/") ###
    Process("cp sw/target/scala-2.12/sw-opt.js.map src/main/resources/"))!
}

zonesFilter := {(z: String) => z == "America/Denver" || z == "America/Mountain"}

lazy val sw = (project in file("sw"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.0.0"
    )
  )
