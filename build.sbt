enablePlugins(ScalaJSPlugin)

name := "image-signature"

scalaVersion := "2.12.0"

libraryDependencies  ++= Seq(
  "com.lihaoyi" %%% "utest" % "0.4.4" % "test"
)

scalaJSModuleKind := ModuleKind.CommonJSModule

testFrameworks += new TestFramework("utest.runner.Framework")
