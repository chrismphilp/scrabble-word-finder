name := "scrabble-solver"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies ++= {

  Seq(
    "org.scalaj" %% "scalaj-http" % "2.4.2",
    "io.spray" %% "spray-json" % "1.3.5",
    "org.scalactic" %% "scalactic" % "3.2.2",
    "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    "junit" % "junit" % "4.13"
  )
}
