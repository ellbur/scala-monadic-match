
organization := "com.github.ellbur"

name := "monadic-match"

scalaVersion := "2.10.1-RC3"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalacOptions ++= Seq(
    "-feature"
)

javacOptions ++= Seq(
    "-source", "-1.6", "-target", "-1.6"
)

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "6.0.4"
)

