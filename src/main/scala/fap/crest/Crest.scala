package fap.crest

import fap.crest.model.{Character, FleetMember}
import fap.model.{CharacterID, FleetID}
import org.http4s.Uri
import org.http4s.Uri.uri

import scalaz.{Free, Inject}

sealed trait CrestOp[A]

final case class GetFleetMembers(characterID: CharacterID, fleetID: FleetID) extends CrestOp[List[FleetMember]]

case object SelectedCharacter extends CrestOp[Character]

class Crest[F[_]](implicit I: Inject[CrestOp, F]) {
  private def lift[A](ga: CrestOp[A]) = Free.liftFC(I.inj(ga))

  def selectedCharacter: Free.FreeC[F, Character] = lift(SelectedCharacter)

  def getFleetMembers(characterID: CharacterID, fleetID: FleetID): Free.FreeC[F, List[FleetMember]] =
    lift(GetFleetMembers(characterID, fleetID))
}
object Crest {
  implicit def crests[F[_]](implicit I: Inject[CrestOp, F]) = new Crest
}

sealed trait Server {
  def root: Uri = this match {
    case Tranquility => uri("https://crest-tq.eveonline.com/")
    case Singularity => uri("https://api-sisi.testeveonline.com")
  }
}
case object Tranquility extends Server
case object Singularity extends Server