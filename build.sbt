import org.scalajs.linker.interface.ModuleSplitStyle
import com.typesafe.tools.mima.core._

inThisBuild(
  List(
    homepage := Some(url("https://pityka.github.io/lui/")),
    licenses := List(("MIT", url("https://opensource.org/licenses/MIT"))),
    developers := List(
      Developer(
        "pityka",
        "Istvan Bartha",
        "bartha.pityu@gmail.com",
        url("https://github.com/pityka/lui")
      )
    ),
  )
)

lazy val scalaVersion213 = "2.13.12"
lazy val scalaVersion3 = "3.3.1"
lazy val scalaVersionInBuild = scalaVersion213

ThisBuild / versionScheme := Some("early-semver")

ThisBuild / versionPolicyIntention := Compatibility.BinaryAndSourceCompatible
ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some(
  raw"^\d+\.\d+\.\d+\+\d+".r
) // Support for versions generated by sbt-dynver

lazy val commonSettings = Seq(
  crossScalaVersions := Seq(scalaVersion213, scalaVersion3),
  scalaVersion := scalaVersionInBuild,
  Test / parallelExecution := false,
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:postfixOps",
        "-language:existentials",
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xfatal-warnings" // Fail the compilation if there are any warnings.
      )
    case Some((2, _)) =>
      Seq(
        "-opt:l:method",
        "-opt:l:inline",
        "-opt-inline-from:org.saddle.**",
        "-opt-warnings",
        "-Wopt",
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:postfixOps",
        "-language:existentials",
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xfatal-warnings", // Fail the compilation if there are any warnings.
        "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
        "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
        "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
        "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
        "-Xlint:option-implicit", // Option.apply used implicit view.
        "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
        "-Ywarn-dead-code", // Warn when dead code is identified.
        // "-Ywarn-numeric-widen", // Warn when numerics are widened.
        "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
        "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
        "-Ywarn-unused:locals", // Warn if a local definition is unused.
        "-Ywarn-unused:params", // Warn if a value parameter is unused.
        "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
        "-Ywarn-unused:privates" // Warn if a private member is unused.
      )
    case _ => ???
  }),
  Compile / console / scalacOptions ~= (_ filterNot (_ == "-Xfatal-warnings"))
) ++ Seq(
  organization := "io.github.pityka",
  licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
  Global / cancelable := true
)

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false
  )
  .aggregate(example, core, stack)

lazy val example = project
  .in(file("example"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("example")))
    }
  )
  .dependsOn(stack)

lazy val core = project
  .in(file("core"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "lui-core",
    libraryDependencies ++= List(
      "org.scala-js" %%% "scalajs-dom" % "2.4.0",
      "com.raquo" %%% "laminar" % "16.0.0"
    )
  )

lazy val stack = project
  .in(file("stack"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "lui-stack"
  )
  .dependsOn(core)
