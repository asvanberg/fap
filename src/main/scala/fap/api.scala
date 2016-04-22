package fap

import java.time.Instant

import _root_.argonaut._
import argonaut.Argonaut._
import fap.crest.interpreter.CrestResponse
import fap.hi._
import fap.hi.statistics.{FleetParticipation, FleetParticipations}
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
  implicit val fleetParticipationCodecJson: CodecJson[FleetParticipation] =
    casecodec3(FleetParticipation.apply, FleetParticipation.unapply)("fleet", "ship", "solarSystem")
  implicit val fleetParticipations: CodecJson[FleetParticipations] =
    casecodec2(FleetParticipations.apply, FleetParticipations.unapply)("characterID", "participations")
  implicit def jsonEncoderOf[A: EncodeJson]: EntityEncoder[A] = org.http4s.argonaut.jsonEncoderOf[A]

  implicit val instantQueryParamDecoder: QueryParamDecoder[Instant] =
    QueryParamDecoder.decodeBy(Instant.ofEpochMilli)
  implicit val instantQueryParam: QueryParam[Instant] = QueryParam.fromKey("since")
  object Since extends OptionalQueryParamMatcher[Instant]

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
      case GET -> Root / "fleets" / LongVar(fleetID) / "members" =>
        respond(members[Fap](FleetID(fleetID)).map(_.fold(Forbidden())(Ok(_))))
      case GET -> Root / "fleets" / "participation" =>
        respond(myParticipations[Fap].map(Ok(_)))
      case GET -> Root / "fleets" / "corporation" =>
        respond(corporationFleets[Fap].map(Ok(_)))
      case request @ POST -> Root / "fleets" =>
        token => request.decode[FleetRegistration] {
          case FleetRegistration(name, FleetURI(fleetId)) =>
            val response = registerFleet[Fap](FleetID(fleetId.toLong), name, Instant.now(), None).map {
              case (fleet, members) =>
                Ok(("fleet", fleet.asJson) ->: ("members", members.asJson) ->: jEmptyObject)
            }
            respond(response)(token)
          case _ => BadRequest()
        }
      case request @ POST -> Root / "fleets" / "corporation" =>
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

    def statisticsService = authenticatedService {
      case GET -> Root / "participation" :? Since(since) =>
        respond(statistics.participations[Fap](since).map(Ok(_)))
      case GET -> Root / "participation" / "corporation" :? Since(since) =>
        respond(statistics.corporationParticipations[Fap](since).map(Ok(_)))
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
