val ScalaVer = "2.12.2"

val Cats = "0.9.0"
val KindProjector = "0.9.4"

lazy val commonSettings = Seq(
  name    := "pure-func-cats"
, version := "0.1.0"
, scalaVersion := ScalaVer
, libraryDependencies ++= Seq(
    "org.typelevel"  %% "cats" % Cats
  )
, scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:experimental.macros",
      "-unchecked",
      // "-Xfatal-warnings",
      // "-Xlint",
      // "-Yinline-warnings",
      "-Ywarn-dead-code",
      "-Xfuture",
      "-Ypartial-unification")
, addCompilerPlugin("org.spire-math" %% "kind-projector" % KindProjector)
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(
    initialCommands := "import purefunccats._"
  )
