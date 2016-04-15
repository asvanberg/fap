name := "fap"

version := "0.1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

lazy val fap = project.in(file(".")).enablePlugins(SbtTwirl)

val http4sVersion = "0.13.2"
val scalazVersion = "7.1.7"
val doobieVersion = "0.2.3"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "io.argonaut" %% "argonaut" % "6.1",
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-contrib-hikari" % doobieVersion,
  "org.tpolecat" %% "doobie-contrib-specs2" % doobieVersion % Test,
  "org.postgresql" % "postgresql" % "9.4.1208",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-argonaut" % http4sVersion,
  "org.http4s" %% "http4s-client" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-twirl" % http4sVersion,
  "com.h2database" % "h2" % "1.4.190",
  "org.flywaydb" % "flyway-core" % "4.0",
  "org.slf4j" % "slf4j-simple" % "1.7.12",
  "org.specs2" %% "specs2-core" % "3.6.6" % Test
)

wartremoverErrors in (Compile, compile) ++= Warts.allBut(Wart.AsInstanceOf, Wart.Throw, Wart.Nothing, Wart.Any, Wart.Product, Wart.IsInstanceOf, Wart.Serializable, Wart.ExplicitImplicitTypes, Wart.NonUnitStatements)
