package fap

import argonaut.Argonaut._
import fap.crest.Server
import fap.hi._
import org.http4s._
import org.http4s.argonaut._
import org.http4s.dsl._
import org.http4s.headers.Authorization
import org.http4s.twirl._

import scalaz.Free.FreeC
import scalaz.concurrent.Task
import scalaz.syntax.bind._
import scalaz.{Free, Kleisli, ~>}

object api {
  class Backend(interpreter: Fap ~> FapTask) {

    private def respond(response: FreeC[Fap, Task[Response]])(token: OAuth2BearerToken): Task[Response] = {
      Free.runFC(response)(interpreter).run(token).join
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
            case _ =>
              Task.now(Response(Unauthorized))
          }
      }
  }

  class Frontend(crestServer: Server) {
    def service = HttpService {
      case GET -> Root => Ok(fap.html.index("Test"))
    }
  }
}
