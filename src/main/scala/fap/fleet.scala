package fap

import java.time.Instant

import doobie.imports._
import fap.model._

import scalaz.~>

object fleet {
  sealed trait FleetOp[A]

  final case class RegisterFleet(name: String, commander: CharacterID, members: List[FleetMember]) extends FleetOp[Fleet]

  final case class MyFleets(characterID: CharacterID) extends FleetOp[List[Fleet]]

  final case class MyParticipations(characterID: CharacterID) extends FleetOp[List[Fleet]]

  final case class FleetMembers(fleetID: FleetID) extends FleetOp[List[FleetMember]]

  object interpreter extends (FleetOp ~> ConnectionIO)  {
    final case class FleetMemberRow(fleetID: FleetID, name: String, commander: CharacterID, logged: Instant, member: CharacterID, solarSystem: SolarSystem, ship: Ship)

    override def apply[A](fa: FleetOp[A]): ConnectionIO[A] = fa match {
      case RegisterFleet(name, commander, members) =>
      case MyFleets(CharacterID(characterID)) =>
        queries.selectFleetMember(characterID)
          .list
    }

    object queries {
      def selectFleetMember(characterID: Long): Query0[FleetMemberRow] =
        sql"""
          SELECT id, name, commander, logged
          FROM fleet
          WHERE commander = ${characterID}
          """
          .query[Fleet]
    }
  }
}
