import sbt.Keys.publishMavenStyle

name := "validator"

version := "0.1.7"

scalaVersion := "2.13.5"

scalacOptions ++= Seq( "-deprecation", "-feature", "-unchecked", "-language:postfixOps", "-language:implicitConversions", "-language:existentials", "-language:dynamics")

organization := "com.vinctus"

githubOwner := "vinctustech"

githubRepository := "validator"

Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

enablePlugins(ScalaJSPlugin)

Test / scalaJSUseMainModuleInitializer := true

Test / scalaJSUseTestModuleInitializer := false

scalaJSUseMainModuleInitializer := true

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()

libraryDependencies ++= Seq(
//  "org.scala-lang.modules" %%% "scala-xml" % "1.2.0"
)

libraryDependencies ++= Seq(
  "org.scalatest" %%% "scalatest" % "3.1.1" % "test"
  //"org.scalacheck" %%% "scalacheck" % "1.14.1" % "test"
)

libraryDependencies ++= Seq(
  "io.github.cquiroz" %%% "scala-java-time" % "2.0.0"
)

Test / mainClass := Some( s"${organization.value}.${name.value.replace('-', '_')}.Main" )

publishMavenStyle := true

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra :=
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
    </developer>
  </developers>
