package fap

import java.time.Instant

import doobie.imports._
import fap.model._

import scalaz.{Free, Inject, ~>}
import scalaz.syntax.functor._

object fleet {
  sealed trait FleetOp[A]
  final case class AddFleet(fleet: FleetID, name: String, commander: CharacterID, logged: Instant) extends FleetOp[Fleet]
  final case class AddFleetMember(fleet: FleetID, member: CharacterID, solarSystem: SolarSystem, ship: Ship) extends FleetOp[FleetMember]
  final case class MyFleets(characterID: CharacterID) extends FleetOp[List[Fleet]]
  final case class MyParticipations(characterID: CharacterID) extends FleetOp[List[Fleet]]
  final case class FleetMembers(fleetID: FleetID) extends FleetOp[List[FleetMember]]

  class Fleets[F[_]](implicit I: Inject[FleetOp, F]) {
    private def lift[A](ga: FleetOp[A]) = Free.liftFC(I.inj(ga))

    def addFleet(fleet: FleetID, name: String, commander: CharacterID, logged: Instant): Free.FreeC[F, Fleet] =
      lift(AddFleet(fleet, name, commander, logged))

    def addMember(fleet: FleetID, member: CharacterID, solarSystem: SolarSystem, ship: Ship): Free.FreeC[F, FleetMember] =
      lift(AddFleetMember(fleet, member, solarSystem, ship))

    def myFleets(commander: CharacterID): Free.FreeC[F, List[Fleet]] =
      lift(MyFleets(commander))

    def myParticipations(member: CharacterID): Free.FreeC[F, List[Fleet]] =
      lift(MyParticipations(member))

    def fleetMembers(fleet: FleetID): Free.FreeC[F, List[FleetMember]] =
      lift(FleetMembers(fleet))
  }

  object Fleets {
    implicit def fleets[F[_]](implicit I: Inject[FleetOp, F]) = new Fleets[F]
  }

  object interpreter extends (FleetOp ~> ConnectionIO)  {
    override def apply[A](fa: FleetOp[A]): ConnectionIO[A] = fa match {
      case AddFleet(fleet, name, commander, logged) =>
        queries.insertFleet(fleet, name, commander, logged)
          .run
          .as(Fleet(fleet, name, commander, logged))
      case AddFleetMember(fleet, member, solarSystem, ship) =>
        queries.insertMember(fleet, member, solarSystem, ship)
          .run
          .as(FleetMember(fleet, member, solarSystem, ship))
      case MyFleets(CharacterID(characterID)) =>
        queries.myFleets(characterID)
          .list
      case MyParticipations(characterID) =>
        queries.myParticipations(characterID)
          .list
      case FleetMembers(fleetID) =>
        queries.fleetMembers(fleetID)
          .list
    }

    object queries {
      implicit val instantMeta: Meta[Instant] = Meta[java.util.Date].nxmap(_.toInstant, java.util.Date.from)

      def insertFleet(fleet: FleetID, name: String, commander: CharacterID, logged: Instant): Update0 =
        sql"""
          INSERT INTO fleet (id, name, commander, logged)
          VALUES ($fleet, $name, $commander, $logged)
          """
          .update

      def insertMember(fleet: FleetID, member: CharacterID, solarSystem: SolarSystem, ship: Ship): Update0 =
        sql"""
          INSERT INTO member (fleet_id, id, solar_system, ship)
          VALUES ($fleet, $member, $solarSystem, $ship)
          """
          .update

      def myFleets(characterID: Long): Query0[Fleet] =
        sql"""
          SELECT id, name, commander, logged
          FROM fleet
          WHERE commander = $characterID
          """
          .query[Fleet]

      def myParticipations(characterID: CharacterID): Query0[Fleet] =
        sql"""
          SELECT fleet.id, name, commander, logged
          FROM member
            INNER JOIN fleet ON fleet.id = member.fleet_id
          WHERE member.id = $characterID
          """
          .query[Fleet]

      def fleetMembers(fleetID: FleetID): Query0[FleetMember] =
        sql"""
          SELECT fleet_id, id, solar_system, ship
          FROM member
          WHERE fleet_id = $fleetID
          """
          .query[FleetMember]
    }
  }
}
