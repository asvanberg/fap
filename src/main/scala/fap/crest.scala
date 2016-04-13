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

  def interpreter(server: Server) = new (CrestOp ~> Kleisli[Task, (Client, OAuth2BearerToken), ?]) {
    override def apply[A](fa: CrestOp[A]): Kleisli[Task, (Client, OAuth2BearerToken), A] =
      Kleisli.kleisli {
        case (client, token) =>
          fa match {
            case GetFleetMembers(CharacterID(characterID), FleetID(fleetID)) =>
              val req = Request(
                uri = server.root / "characters" / characterID.toString / "fleets" / fleetID.toString,
                headers = Headers(Authorization(token))
              )
              client.fetchAs[List[FleetMember]](req)(jsonOf)
            case SelectedCharacter =>
              val req = Request(
                uri = server.root / "decode",
                headers = Headers(Authorization(token))
              )
              client.fetchAs[CharacterID](req)(jsonOf)
          }
      }
  }
}
