package fap

import java.time.Instant

import argonaut.CodecJson
import argonaut.CodecJson._

object model {
  final case class CharacterID(id: Long) extends AnyVal

  object CharacterID {
    implicit val jsonCodec: CodecJson[CharacterID] = CodecJson.derived[Long].xmap(CharacterID(_))(_.id)
  }

  type SolarSystem = String

  type Ship = String

  final case class Fleet(fleetID: FleetID, name: String, commander: CharacterID, logged: Instant)

  final case class FleetMember(fleetID: FleetID, characterID: CharacterID, solarSystem: SolarSystem, ship: Ship)

  object FleetMember {
    implicit val jsonCodec: CodecJson[FleetMember] = casecodec4(FleetMember.apply, FleetMember.unapply)("fleetID", "characterID", "solarSystem", "ship")
  }

  final case class FleetID(id: Long) extends AnyVal
}
