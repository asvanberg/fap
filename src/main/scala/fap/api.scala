package fap

import argonaut.Argonaut._
import doobie.imports._
import fap.hi._
import org.http4s._
import org.http4s.argonaut._
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.dsl._
import org.http4s.headers.Authorization

import scalaz.Free.FreeC
import scalaz.concurrent.Task
import scalaz.syntax.bind._
import scalaz.{Free, Kleisli, ~>}

object api {
  class Backend(interpreter: Fap ~> FapTask) {

    private def respond(response: FreeC[Fap, Task[Response]])(token: OAuth2BearerToken): Task[Response] = {
      val client = PooledHttp1Client()
      Free.runFC(response)(interpreter).apply((client, token)).join ensuring client.shutdown
    }

    def service = authenticatedService {
      case GET -> Root / "fleets" =>
        respond(myFleets[Fap].map(fleets => Ok(fleets.asJson)))
      case GET -> Root / "participations" =>
        respond(myParticipations[Fap].map(fleets => Ok(fleets.asJson)))
    }

    private def authenticatedService(run: PartialFunction[Request, OAuth2BearerToken => Task[Response]]): HttpService =
      Kleisli {
        request =>
          request.headers.get(Authorization) match {
            case Some(Authorization(token@OAuth2BearerToken(_))) =>
              HttpService(run.andThen(_(token)))(request)
            case None =>
              Task.now(Response(Unauthorized))
          }
      }
  }
}
