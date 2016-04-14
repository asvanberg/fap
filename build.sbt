name := "fap"

version := "0.1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.7",
  "org.scalaz" %% "scalaz-concurrent" % "7.1.7",
  "org.scalaz" %% "scalaz-effect" % "7.1.7",
  "io.argonaut" %% "argonaut" % "6.1",
  "org.tpolecat" %% "doobie-core" % "0.2.3",
  "org.tpolecat" %% "doobie-contrib-hikari" % "0.2.3",
  "org.tpolecat" %% "doobie-contrib-specs2" % "0.2.3" % Test,
  "org.postgresql" % "postgresql" % "9.4.1208",
  "org.http4s" %% "http4s-dsl" % "0.13.1",
  "org.http4s" %% "http4s-blaze-server" % "0.13.1",
  "org.http4s" %% "http4s-argonaut" % "0.13.1",
  "org.http4s" %% "http4s-client" % "0.13.1",
  "org.http4s" %% "http4s-blaze-client" % "0.13.1",
  "com.h2database" % "h2" % "1.4.190",
  "org.flywaydb" % "flyway-core" % "4.0",
  "org.slf4j" % "slf4j-simple" % "1.7.12"
)

wartremoverErrors in (Compile, compile) ++= Warts.allBut(Wart.AsInstanceOf, Wart.Throw)
