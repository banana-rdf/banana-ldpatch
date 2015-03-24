name in ThisBuild := "banana-ldpatch"

organization in ThisBuild := "org.w3"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.5"

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation", "-unchecked")

lazy val root = project.in(file("."))
  .aggregate(jena, ldpatchJs)
  .settings(
    publish := {},
    publishLocal := {}
  )

val bananaV = "0.8.1"

scalaJSStage in Global := FastOptStage

testFrameworks in ThisBuild += new TestFramework("utest.runner.Framework")

lazy val ldpatch = crossProject
  .crossType(CrossType.Pure)
  .in(file("ldpatch"))
  .settings(
    name := "banana-ldpatch",
    libraryDependencies += "org.w3" %%% "banana-rdf" % bananaV,
    resolvers += "bintray-alexander_myltsev" at "http://dl.bintray.com/content/alexander-myltsev/maven",
    libraryDependencies += "org.parboiled" %%% "parboiled" % "2.0.1",
    resolvers += Resolver.url("inthenow-releases", url("http://dl.bintray.com/inthenow/releases"))(Resolver.ivyStylePatterns),
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % "test"
  )

lazy val ldpatchJvm = ldpatch.jvm

lazy val ldpatchJs = ldpatch.js

lazy val jena = project
  .in(file("jena"))
  .settings(
    name := "banana-ldpatch-jena",
    libraryDependencies += "org.w3" %% "banana-jena" % bananaV
  )
  .dependsOn(ldpatchJvm % "test->test;compile->compile")
