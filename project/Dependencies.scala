import sbt._

object Dependencies {

  lazy val ZioVersion = "2.1.15"
  lazy val CirceVersion = "0.14.2"
  lazy val ScalaTest = "org.scalatest" %% "scalatest" % "3.2.19"
  lazy val CatsCore = "org.typelevel" %% "cats-core" % "2.13.0"
  lazy val CatsEffect = "org.typelevel" %% "cats-effect" % "3.4.5"
  lazy val ZIO: Seq[ModuleID] = Seq("dev.zio" %% "zio" % ZioVersion,
                     "dev.zio" %% "zio-test" % ZioVersion  % Test,
                     "dev.zio" %% "zio-test-sbt" % ZioVersion  % Test,
                     "dev.zio" %% "zio-test-magnolia" % ZioVersion % Test)

  lazy val fs2: Seq[ModuleID] = Seq(
    "co.fs2" %% "fs2-core" % "3.6.1",
    "co.fs2" %% "fs2-io"   % "3.6.1"
  )

  lazy val ZioConfig: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio-config" % "4.0.2",
    "dev.zio" %% "zio-config-magnolia" % "4.0.2",
    "dev.zio" %% "zio-config-typesafe" % "4.0.2"
  )
  lazy val http4s: Seq[ModuleID] = Seq(
    "org.http4s" %% "http4s-client" % "0.23.18",
    "org.http4s" %% "http4s-dsl" % "0.23.18",
    "org.http4s" %% "http4s-ember-server" % "0.23.18",
    "org.http4s" %% "http4s-ember-client" % "0.23.18",

  )

  lazy val circe = Seq(
    "io.circe" %% "circe-core" % CirceVersion,
    "io.circe" %% "circe-generic" % CirceVersion,
    "io.circe" %% "circe-parser" % CirceVersion,
    "io.circe" %% "circe-derivation" % "0.13.0-M4",
    "org.http4s" %% "http4s-circe" % "0.23.14"
  )


}
