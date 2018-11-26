name := "tarif_parser"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "com.google.code.gson" % "gson" % "1.7.1"

libraryDependencies ++= Seq(
  "org.apache.poi" % "poi" % "3.15-beta2",
  "org.apache.poi" % "poi-ooxml" % "3.15-beta2",
  "org.apache.poi" % "poi-ooxml-schemas" % "3.15-beta2"
)
