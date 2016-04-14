package fap

import argonaut.Argonaut._
import doobie.imports._
import fap.crest._
import fap.fleet._
import fap.hi._
import org.http4s._
import org.http4s.argonaut._
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.dsl._
import org.http4s.headers.Authorization

import scalaz.Free.FreeC
import scalaz.concurrent.Task
import scalaz.syntax.bind._
import scalaz.syntax.kleisli._
import scalaz.{Free, Kleisli, ~>}

object api extends App {

  val crestServer = Tranquility

  val xa = DriverManagerTransactor[Task]("org.postgresql.Driver", "url", "user", "pass")

  val fleetInterpreter: FleetOp ~> FapTask = new (Task ~> FapTask) {
    override def apply[A](fa: Task[A]): FapTask[A] = fa.liftKleisli
  }.compose(xa.trans.compose(fleet.interpreter))

  val crestInterpreter: CrestOp ~> FapTask = crest.interpreter(crestServer)

  val interpreter = new (Fap ~> FapTask) {
    override def apply[A](fa: Fap[A]): FapTask[A] =
      fa.run.fold(crestInterpreter.apply, fleetInterpreter.apply)
  }


  class Backend(interpreter: Fap ~> FapTask) {

    def run(response: FreeC[Fap, Task[Response]], token: OAuth2BearerToken): Task[Response] = {
      val client = PooledHttp1Client()
      Free.runFC(response)(interpreter).apply((client, token)).join ensuring client.shutdown
    }

    def service = authenticatedService {
      token => HttpService {
        case GET -> Root / "fleets" =>
          val response = myFleets[Fap].map(fleets => Ok(fleets.asJson))
          run(response, token)
      }
    }

    def authenticatedService(run: OAuth2BearerToken => HttpService): HttpService =
      Kleisli {
        request =>
          request.headers.get(Authorization) match {
            case Some(Authorization(token@OAuth2BearerToken(_))) =>
              run(token)(request)
            case None =>
              Task.now(Response(Unauthorized))
          }
      }
  }
}
