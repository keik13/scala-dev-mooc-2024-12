
scalaVersion := "2.13.16"


name := "scala-dev-mooc-2024-12"
organization := "ru.otus"
version := "1.0"

libraryDependencies += Dependencies.ScalaTest
libraryDependencies += Dependencies.CatsCore
libraryDependencies += Dependencies.CatsEffect
libraryDependencies ++= Dependencies.ZIO
libraryDependencies ++= Dependencies.ZioConfig
libraryDependencies ++= Dependencies.fs2
libraryDependencies ++= Dependencies.http4s
libraryDependencies ++= Dependencies.circe
