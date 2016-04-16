package fap

import java.time.LocalDateTime
import java.time.format.DateTimeParseException

import _root_.argonaut._, Argonaut._
import fap.crest.interpreter.FleetMember
import fap.model.{FleetMember => _, _}
import org.http4s.Uri._
import org.http4s._
import org.http4s.argonaut._
import org.http4s.client.Client
import org.http4s.headers.Authorization

import scalaz.concurrent.Task
import scalaz.syntax.id._
import scalaz.{Kleisli, _}
import scala.util.control.Exception.catching

object crest {
  sealed trait Server {
    def root: Uri = this match {
      case Tranquility => uri("https://crest-tq.eveonline.com/")
      case Singularity => uri("https://api-sisi.testeveonline.com")
    }
  }
  case object Tranquility extends Server
  case object Singularity extends Server

  sealed trait CrestOp[A]

  final case class GetFleetMembers(characterID: CharacterID, fleetID: FleetID) extends CrestOp[List[FleetMember]]

  case object SelectedCharacter extends CrestOp[CharacterID]

  type CrestIO[A] = Free.FreeC[CrestOp, A]

  class Crest[F[_]](implicit I: Inject[CrestOp, F]) {
    private def lift[A](ga: CrestOp[A]) = Free.liftFC(I.inj(ga))

    def selectedCharacter: Free.FreeC[F, CharacterID] = lift(SelectedCharacter)

    def getFleetMembers(characterID: CharacterID, fleetID: FleetID): Free.FreeC[F, List[FleetMember]] =
      lift(GetFleetMembers(characterID, fleetID))
  }
  object Crest {
    implicit def crests[F[_]](implicit I: Inject[CrestOp, F]) = new Crest
  }

  object interpreter {
    def apply(client: Client, server: Server) = new (CrestOp ~> Kleisli[Task, OAuth2BearerToken, ?]) {
      override def apply[A](fa: CrestOp[A]): Kleisli[Task, OAuth2BearerToken, A] =
        Kleisli.kleisli {
          token =>
            fa match {
              case GetFleetMembers(CharacterID(characterID), FleetID(fleetID)) =>
                val req = Request(
                  uri = server.root / "fleets" / fleetID.toString / "",
                  headers = Headers(Authorization(token))
                )
                client.fetchAs[List[FleetMember]](req)
              case SelectedCharacter =>
                val req = Request(
                  uri = server.root / "decode" / "",
                  headers = Headers(Authorization(token))
                )
                for {
                  characterLocation <- client.fetchAs[Decode](req)
                  character <- client.getAs[Character](characterLocation.href)
                } yield character.id
            }
        }
    }

    final case class Decode(href: Uri)
    final case class Character(id: CharacterID)
    final case class FleetMember(
                                  squadID: Long,
                                  solarSystem: FleetMember.SolarSystem,
                                  wingID: Long,
                                  roleID: Long,
                                  character: FleetMember.Character,
                                  boosterID: Long,
                                  boosterName: String,
                                  roleName: String,
                                  station: Option[FleetMember.Station],
                                  ship: FleetMember.Ship,
                                  joinTime: LocalDateTime
                                )
    object FleetMember {
      final case class SolarSystem(id: Long, name: String, href: Uri)
      final case class Station(id: Long, name: String, href: Uri)
      final case class Ship(id: Long, name: String, href: Uri)
      final case class Character(id: CharacterID, name: String, href: Uri)
    }

    implicit def jsonEntityDecoder[A: DecodeJson]: EntityDecoder[A] = jsonOf
    implicit val uriDecodeJson: DecodeJson[Uri] = DecodeJson.optionDecoder(json =>
      json.string.flatMap(fromString(_).toOption),
      "Uri"
    )
    implicit val uriEncodeJson: EncodeJson[Uri] = EncodeJson(_.renderString |> jString)
    implicit val localDateEncodeJson: EncodeJson[LocalDateTime] = EncodeJson(_.toString |> jString)
    implicit val localDateDecodeJson: DecodeJson[LocalDateTime] = DecodeJson.optionDecoder(json =>
      json.string.flatMap(str => catching(classOf[DateTimeParseException]).opt(LocalDateTime.parse(str))),
      "LocalDateTime"
    )
    implicit val decodeDecodeJson: DecodeJson[Decode] = DecodeJson(cursor =>
      (cursor --\ "character" --\ "href").as[Uri].map(Decode(_)))
    implicit val characterDecodeJson: DecodeJson[Character] = casecodec1(Character.apply, Character.unapply)("id")
    implicit val solarSystemJson: CodecJson[FleetMember.SolarSystem] =
      casecodec3(FleetMember.SolarSystem.apply, FleetMember.SolarSystem.unapply)("id", "name", "href")
    implicit val stationJson: CodecJson[FleetMember.Station] =
      casecodec3(FleetMember.Station.apply, FleetMember.Station.unapply)("id", "name", "href")
    implicit val shipJson: CodecJson[FleetMember.Ship] =
      casecodec3(FleetMember.Ship.apply, FleetMember.Ship.unapply)("id", "name", "href")
    implicit val fmCharacterJson: CodecJson[FleetMember.Character] =
      casecodec3(FleetMember.Character.apply, FleetMember.Character.unapply)("id", "name", "href")
    implicit val fleetMemberJson: CodecJson[FleetMember] =
      casecodec11(FleetMember.apply, FleetMember.unapply)("squadID", "solarSystem", "wingID", "roleID", "character", "boosterID", "boosterName", "roleName", "station", "ship", "joinTime")
  }
}
