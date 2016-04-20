package fap

import java.time.Instant

import doobie.imports.{freeMonadC, unapplyMMFA}
import fap.crest.Crest
import fap.crest.model.Character
import fap.fleet.Fleets
import fap.model.{CorporationID, Fleet, FleetID, FleetMember}

import scalaz.Free
import scalaz.std.list._
import scalaz.syntax.traverse._

object hi {
  def myFleets[F[_]](implicit C: Crest[F], F: Fleets[F]): Free.FreeC[F, List[Fleet]] =
    C.selectedCharacter map (_.id) >>= F.myFleets

  def myParticipations[F[_]](implicit C: Crest[F], F: Fleets[F]): Free.FreeC[F, List[Fleet]] =
    C.selectedCharacter map (_.id) >>= F.myParticipations

  def registerFleet[F[_]](fleetID: FleetID, name: String, logged: Instant, corporation: Option[CorporationID])(implicit C: Crest[F], F: Fleets[F]): Free.FreeC[F, (Fleet, List[FleetMember])] =
    for {
      commander <- C.selectedCharacter
      fleetMembers <- C.getFleetMembers(fleetID)
      fleet <- F.addFleet(fleetID, name, commander.id, logged, corporation)
      members <- fleetMembers traverseU { member =>
        F.addMember(fleetID, member.character.id, member.solarSystem.name, member.ship.name)
      }
    } yield (fleet, members)

  def currentCharacter[F[_]](implicit C: Crest[F]): Free.FreeC[F, Character] =
    C.selectedCharacter

  def corporationFleets[F[_]](implicit C: Crest[F], F: Fleets[F]): Free.FreeC[F, List[Fleet]] =
    currentCharacter map (_.corporation.id) >>= F.corporationFleets
}
