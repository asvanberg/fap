package fap

import doobie.syntax.catchable.ToDoobieCatchableOps._
import fap.model._
import org.http4s.{Headers, OAuth2BearerToken, Request, Uri}
import Uri._
import org.http4s.argonaut._
import org.http4s.client.Client
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.headers.Authorization

import scalaz.concurrent.Task
import scalaz.{Kleisli, _}

object crest {
  sealed trait Crest {
    def root: Uri = this match {
      case Tranquility => uri("https://crest-tq.eveonline.com/")
      case Singularity => uri("https://sisi-api.testeveonline.com")
    }
  }
  case object Tranquility extends Crest
  case object Singularity extends Crest

  sealed trait CrestOp[A]

  final case class GetFleetMembers(characterID: CharacterID, fleetID: FleetID) extends CrestOp[List[FleetMember]]

  case object SelectedCharacter extends CrestOp[CharacterID]

  type CrestIO[A] = Free.FreeC[CrestOp, A]

  def getFleetMembers(characterID: CharacterID, fleetID: FleetID): CrestIO[List[FleetMember]] =
    Free.liftFC(GetFleetMembers(characterID, fleetID))

  object interpreter extends (CrestOp ~> Kleisli[Task, (Client, Crest, OAuth2BearerToken), ?]) {
    override def apply[A](fa: CrestOp[A]): Kleisli[Task, (Client, Crest, OAuth2BearerToken), A] =
      Kleisli.kleisli {
        case (client, crest, token) =>
          fa match {
            case GetFleetMembers(CharacterID(characterID), FleetID(fleetID)) =>
              val req = Request(
                uri = crest.root / "characters" / characterID.toString / "fleets" / fleetID.toString,
                headers = Headers(Authorization(token))
              )
              client.fetchAs[List[FleetMember]](req)(jsonOf)
            case SelectedCharacter =>
              val req = Request(
                uri = crest.root / "decode",
                headers = Headers(Authorization(token))
              )
              client.fetchAs[CharacterID](req)(jsonOf)
          }
      }
  }

  def pooled[A](prg: CrestIO[A]): Kleisli[Task, (Crest, OAuth2BearerToken), A] =
    Kleisli.kleisli {
      case (crest, token) =>
        val client = PooledHttp1Client()
        val interpreted = Free.runFC[CrestOp, Kleisli[Task, (Client, Crest, OAuth2BearerToken), ?], A](prg)(interpreter)
        interpreted(client, crest, token) ensuring client.shutdown
    }
}
