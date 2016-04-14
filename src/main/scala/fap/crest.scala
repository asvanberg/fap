package fap

import _root_.argonaut._, Argonaut._
import fap.model._
import org.http4s._
import org.http4s.Uri._
import org.http4s.argonaut._
import org.http4s.client.Client
import org.http4s.headers.Authorization

import scalaz.concurrent.Task
import scalaz.{Kleisli, _}

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
                  uri = server.root / "characters" / characterID.toString / "fleets" / fleetID.toString / "",
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

    implicit def jsonEntityDecoder[A: DecodeJson]: EntityDecoder[A] = jsonOf
    implicit val uriDecodeJson: DecodeJson[Uri] = DecodeJson.optionDecoder(json =>
      json.string.flatMap(fromString(_).toOption),
      "Uri"
    )
    implicit val decodeDecodeJson: DecodeJson[Decode] = DecodeJson(cursor =>
      (cursor --\ "character" --\ "href").as[Uri].map(Decode(_)))
    implicit val characterDecodeJson: DecodeJson[Character] = casecodec1(Character.apply, Character.unapply)("id")
  }
}
