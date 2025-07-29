//ThisBuild / version := "0.1.0-SNAPSHOT"
//
//ThisBuild / scalaVersion := "2.13.8"
//
//lazy val root = (project in file("."))
//  .settings(
//      scalaVersion := "2.13.7",
//      addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.0" cross CrossVersion.full),
//    libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.0",
//    libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.5.0",
//      // We also recommend using chiseltest for writing unit tests
//      libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.0" % "test",
//    name := "Padding-3D-FFT-Chisel-HCL"
//  )

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"


val chiselVersion = "6.0.0"

lazy val root = (project in file("."))
  .settings(
      name := "Padding-3D-FFT-Chisel-HCL",
      addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full),
      libraryDependencies += "org.chipsalliance" %% "chisel" % chiselVersion,
      libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "6.0.0"
  )