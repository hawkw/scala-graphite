name := "graphite"

version := "1.0"

scalaVersion := "2.12.1"

val scalatestVersion = "3.0.1"
val spireVersion = "0.3.1"

libraryDependencies ++= Seq(
//  "org.spire-math" % "algebra_2.11"     % spireVersion
//  , "org.spire-math" % "algebra-std_2.11" % spireVersion
    "org.scalactic"  %% "scalactic"       % scalatestVersion
//  , "org.scalacheck" %% "scalacheck"      % "1.13.0"          % "test"
  , "org.scalatest"  %% "scalatest"       % scalatestVersion  % "test"
)