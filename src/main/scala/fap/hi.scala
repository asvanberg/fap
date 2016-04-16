package fap

import java.time.Instant

import doobie.imports.{freeMonadC, unapplyMMFA}
import fap.crest.Crest
import fap.fleet.Fleets
import fap.model.{Fleet, FleetID, FleetMember}

import scalaz.Free
import scalaz.std.list._
import scalaz.syntax.traverse._

object hi {
  def myFleets[F[_]](implicit C: Crest[F], F: Fleets[F]): Free.FreeC[F, List[Fleet]] =
    C.selectedCharacter >>= F.myFleets

  def myParticipations[F[_]](implicit C: Crest[F], F: Fleets[F]): Free.FreeC[F, List[Fleet]] =
    C.selectedCharacter >>= F.myParticipations

  def registerFleet[F[_]](fleetID: FleetID, name: String, logged: Instant)(implicit C: Crest[F], F: Fleets[F]): Free.FreeC[F, (Fleet, List[FleetMember])] =
    for {
      commander <- C.selectedCharacter
      fleetMembers <- C.getFleetMembers(commander, fleetID)
      fleet <- F.addFleet(fleetID, name, commander, logged)
      members <- fleetMembers traverseU { member =>
        F.addMember(fleetID, member.character.id, member.solarSystem.name, member.ship.name)
      }
    } yield (fleet, members)
}
