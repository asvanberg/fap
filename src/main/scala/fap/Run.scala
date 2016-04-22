package fap

import doobie.contrib.hikari.hikaritransactor.HikariTransactor
import doobie.imports._
import fap.api.Backend
import fap.crest.interpreter.CrestResponseT
import fap.crest.{CrestOp, Server, Singularity, Tranquility}
import fap.fleet.FleetOp
import org.flywaydb.core.Flyway
import org.http4s.client.Client
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.server.blaze.BlazeBuilder

import scala.util.Properties._
import scala.Function.const
import scalaz._
import scalaz.concurrent.Task
import scalaz.std.string._
import scalaz.syntax.foldable1._
import scalaz.syntax.kleisli._
import scalaz.syntax.std.option._
import scalaz.syntax.apply._
import scalaz.syntax.std.string._
import scalaz.Validation._

object Run extends App {

  val app = for {
    config         <- loadConfig
    _              <- migrateDatabase(config.database)
    connectionPool <- createPool(config.database)
    crestClient    =  PooledHttp1Client()
    interpreter    =  createInterpreter(connectionPool.trans, crestClient, config.crest)
    server         <- createServer(config, interpreter)
    cleanUp        =  createCleanUpTask(connectionPool, crestClient, server)
  } yield (server, cleanUp)

  val (server, cleanUp) = app.run

  sys.addShutdownHook(cleanUp.run)

  server.awaitShutdown()

  final case class Configuration(port: Int, database: DatabaseConfiguration, crest: Server)
  final case class DatabaseConfiguration(driver: String, url: String)

  def loadConfig = {
    val crestServer = envOrNone("CREST_SERVER") match {
      case Some("tq") => success(Tranquility)
      case Some("sisi") => success(Singularity)
      case _ => failureNel("CREST_SERVER missing or invalid, must be 'tq' or 'sisi'")
    }
    val port = envOrNone("HTTP_PORT").map(_.parseInt).getOrElse(success(8080)).leftMap(const("Invalid HTTP_PORT specified")).toValidationNel
    val databaseUrl = envOrNone("DATABASE_URL").toSuccessNel("DATABASE_URL not specified")
    val databaseDriver = envOrNone("DATABASE_DRIVER").toSuccessNel("DATABASE_DRIVER not specified")

    val databaseConfiguration = (databaseDriver |@| databaseUrl)(DatabaseConfiguration)
    val configuration = (port |@| databaseConfiguration |@| crestServer)(Configuration)
    configuration.fold(
      errors => Task.fail(new IllegalArgumentException(errors.intercalate1("\n"))),
      Task.now
    )
  }

  def migrateDatabase(databaseConfiguration: DatabaseConfiguration) =
    Task.delay {
      val flyway = new Flyway
      flyway.setDataSource(databaseConfiguration.url, "", "")
      flyway.migrate()
    }

  def createPool(databaseConfiguration: DatabaseConfiguration) =
    HikariTransactor[Task](databaseConfiguration.driver, databaseConfiguration.url, "", "")

  def createInterpreter(databaseInterpreter: ConnectionIO ~> Task, crestClient: Client, crestServer: Server) = {
    val fleetInterpreter: FleetOp ~> FapTask = new (Task ~> FapTask) {
      override def apply[A](fa: Task[A]): FapTask[A] =
        fa.liftKleisli.liftMK[CrestResponseT]
    }.compose(databaseInterpreter.compose(fleet.interpreter))

    val crestInterpreter: CrestOp ~> FapTask = crest.interpreter(crestClient, crestServer)

    new (Fap ~> FapTask) {
      override def apply[A](fa: Fap[A]): FapTask[A] =
        fa.run.fold(crestInterpreter.apply, fleetInterpreter.apply)
    }
  }

  def createServer(configuration: Configuration, interpreter: Fap ~> FapTask) = {
    val backend = new Backend(interpreter)
    BlazeBuilder
      .bindLocal(configuration.port)
      .mountService(backend.statisticsService, "/statistics")
      .mountService(backend.service)
      .start
  }

  def createCleanUpTask(connectionPool: HikariTransactor[Task], crestClient: Client, server: org.http4s.server.Server) = {
    import org.log4s._
    def shutdown(what: String, task: Task[Unit]) =
      Task.delay(getLogger.info(s"Shutting down $what... ")) *> task *> Task.delay(getLogger.info(s"$what shut down"))
    shutdown("CREST client", crestClient.shutdown) *>
      shutdown("Connection pool", connectionPool.shutdown) *>
      shutdown("HTTP server", server.shutdown)
  }
}
