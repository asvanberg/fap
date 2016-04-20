package fap

import java.time.Instant

import argonaut.CodecJson
import argonaut.CodecJson._

import scalaz.Equal
import scalaz.std.anyVal.longInstance

object model {
  final case class CharacterID(id: Long) extends AnyVal

  object CharacterID {
    implicit val jsonCodec: CodecJson[CharacterID] = CodecJson.derived[Long].xmap(CharacterID(_))(_.id)
    implicit val corporationIDEqual: Equal[CharacterID] = Equal[Long].contramap(_.id)
  }

  final case class CorporationID(id: Long) extends AnyVal
  object CorporationID {
    implicit val jsonCodec: CodecJson[CorporationID] = CodecJson.derived[Long].xmap(CorporationID(_))(_.id)
    implicit val corporationIDEqual: Equal[CorporationID] = Equal[Long].contramap(_.id)
  }

  type SolarSystem = String

  type Ship = String

  final case class Fleet(fleetID: FleetID, name: String, commander: CharacterID, logged: Instant, corporation: Option[CorporationID])
  object Fleet {
    implicit val jsonCodec: CodecJson[Fleet] = casecodec5(Fleet.apply, Fleet.unapply)("fleetID", "name", "commander", "logged", "corporationID")
  }

  final case class FleetMember(fleetID: FleetID, characterID: CharacterID, solarSystem: SolarSystem, ship: Ship)

  object FleetMember {
    implicit val jsonCodec: CodecJson[FleetMember] = casecodec4(FleetMember.apply, FleetMember.unapply)("fleetID", "characterID", "solarSystem", "ship")
  }

  final case class FleetID(id: Long) extends AnyVal
  object FleetID {
    implicit val jsonCodec: CodecJson[FleetID] = CodecJson.derived[Long].xmap(FleetID(_))(_.id)
  }
}
