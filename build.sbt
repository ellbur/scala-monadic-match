
organization := "com.github.ellbur"

name := "monadic-match"

version := "0.2-SNAPSHOT"

scalaVersion := "2.10.1-RC3"

scalaSource in Compile <<= baseDirectory(_ / "src")

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "6.0.4"
)

// https://github.com/harrah/xsbt/wiki/Publishing
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

