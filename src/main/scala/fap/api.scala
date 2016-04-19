package fap

import java.time.Instant

import _root_.argonaut._
import argonaut.Argonaut._
import fap.crest.interpreter.CrestResponse
import fap.hi._
import fap.model.FleetID
import org.http4s._
import org.http4s.argonaut._
import org.http4s.dsl._
import org.http4s.headers.Authorization

import scalaz.Free.FreeC
import scalaz.concurrent.Task
import scalaz.{Free, Kleisli, ~>}

object api {
  private val FleetURI = ".*/fleets/(\\d+)/$".r

  final case class FleetRegistration(name: String, href: String)

  implicit val fleetRegistrationJson: CodecJson[FleetRegistration] = casecodec2(FleetRegistration.apply, FleetRegistration.unapply)("name", "href")
  implicit val fleetRegistrationDecoder: EntityDecoder[FleetRegistration] = jsonOf
  implicit def jsonEncoderOf[A: EncodeJson]: EntityEncoder[A] = org.http4s.argonaut.jsonEncoderOf[A]

  class Backend(interpreter: Fap ~> FapTask) {

    private def respond(response: FreeC[Fap, Task[Response]])(token: OAuth2BearerToken): Task[Response] = {
      Free.runFC(response)(interpreter).run(token).run.flatMap {
        case CrestResponse.Ok(r) => r
        case CrestResponse.Unauthorized => Task.now(Response(Unauthorized))
        case CrestResponse.Forbidden => Forbidden()
        case CrestResponse.Error => ServiceUnavailable()
      }
    }

    def service = authenticatedService {
      case GET -> Root / "fleets" =>
        respond(myFleets[Fap].map(Ok(_)))
      case GET -> Root / "participations" =>
        respond(myParticipations[Fap].map(Ok(_)))
      case request @ POST -> Root / "register" =>
        token => request.decode[FleetRegistration] {
          case FleetRegistration(name, FleetURI(fleetId)) =>
            val response = registerFleet[Fap](FleetID(fleetId.toLong), name, Instant.now(), None).map {
              case (fleet, members) =>
                Ok(("fleet", fleet.asJson) ->: ("members", members.asJson) ->: jEmptyObject)
            }
            respond(response)(token)
          case _ => BadRequest()
        }
      case request @ POST -> Root / "register" / "corporation" =>
        token => request.decode[FleetRegistration] {
          case FleetRegistration(name, FleetURI(fleetId)) =>
            val response = for {
              commander <- currentCharacter[Fap]
              x <- registerFleet[Fap](FleetID(fleetId.toLong), name, Instant.now(), Some(commander.corporation.id))
              (fleet, members) = x
            } yield Ok(("fleet", fleet.asJson) ->: ("members", members.asJson) ->: jEmptyObject)
            respond(response)(token)
        }
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
}
