package fap

import java.time.Instant

import doobie.imports.{freeMonadC, unapplyMMFA}
import fap.crest.Crest
import fap.crest.model.Character
import fap.fleet.Fleets
import fap.model._

import scalaz.Free
import scalaz.std.list._
import scalaz.std.map._
import scalaz.syntax.apply.{ToFunctorOps => _, _}
import scalaz.syntax.equal._
import scalaz.syntax.std.boolean._
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

  def members[F[_]](fleetID: FleetID)(implicit C: Crest[F], F: Fleets[F]): Free.FreeC[F, Option[List[FleetMember]]] =
    (currentCharacter |@| F.getFleet(fleetID) |@| F.fleetMembers(fleetID))({
      case (current, fleet, members) =>
        val corpMember = fleet.flatMap(_.corporation).exists(_ === current.corporation.id)
        val fleetMember = members.exists(_.characterID === current.id)
        (corpMember || fleetMember).option(members)
    })

  object statistics {
    final case class FleetParticipation(fleet: Fleet, ship: Ship, solarSystem: SolarSystem)
    final case class FleetParticipations(characterID: CharacterID, participation: List[FleetParticipation])

    def participations[F[_]: Crest : Fleets](since: Option[Instant]): Free.FreeC[F, List[FleetParticipations]] =
      participations0(myFleets, since)

    def corporationParticipations[F[_]: Crest : Fleets](since: Option[Instant]): Free.FreeC[F, List[FleetParticipations]] =
      participations0(corporationFleets, since)

    private def participations0[F[_]](fleetsF: Free.FreeC[F, List[Fleet]], since: Option[Instant])(implicit F: Fleets[F]) =
      for {
        fleets <- fleetsF map { _.filter(fleet => since.forall(fleet.logged.isAfter)) }
        participations <- fleets traverseU { fleet =>
          F.fleetMembers(fleet.fleetID).map(_.strengthL(fleet))
        }
      } yield participations.flatten.foldMap {
        case (fleet, FleetMember(_, character, solarSystem, ship)) =>
          Map(character -> List(FleetParticipation(fleet, ship, solarSystem)))
      }.toList.map((FleetParticipations.apply _).tupled)
  }
}
