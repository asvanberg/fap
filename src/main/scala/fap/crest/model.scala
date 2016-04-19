package fap.crest

import java.time.LocalDateTime
import java.time.format.DateTimeParseException

import argonaut._, Argonaut._
import fap.model.{CharacterID, CorporationID}
import org.http4s.Uri
import org.http4s.Uri._

import scala.util.control.Exception._
import scalaz.syntax.id._

object model {
  final case class Decode(href: Uri)
  final case class Character(id: CharacterID, corporation: Character.Corporation)
  object Character {
    final case class Corporation(id: CorporationID, name: String, href: Uri)
  }
  final case class FleetMembers(members: List[FleetMember])
  final case class FleetMember(
                                squadID: Long,
                                solarSystem: FleetMember.SolarSystem,
                                wingID: Long,
                                roleID: Long,
                                character: FleetMember.Character,
                                boosterID: Long,
                                boosterName: String,
                                roleName: String,
                                station: Option[FleetMember.Station],
                                ship: FleetMember.Ship,
                                joinTime: LocalDateTime
                              )
  object FleetMember {
    final case class SolarSystem(id: Long, name: String, href: Uri)
    final case class Station(id: Long, name: String, href: Uri)
    final case class Ship(id: Long, name: String, href: Uri)
    final case class Character(id: CharacterID, name: String, href: Uri)
  }

  implicit val uriDecodeJson: DecodeJson[Uri] = DecodeJson.optionDecoder(json =>
    json.string.flatMap(fromString(_).toOption),
    "Uri"
  )
  implicit val uriEncodeJson: EncodeJson[Uri] = EncodeJson(_.renderString |> jString)
  implicit val localDateEncodeJson: EncodeJson[LocalDateTime] = EncodeJson(_.toString |> jString)
  implicit val localDateDecodeJson: DecodeJson[LocalDateTime] = DecodeJson.optionDecoder(json =>
    json.string.flatMap(str => catching(classOf[DateTimeParseException]).opt(LocalDateTime.parse(str))),
    "LocalDateTime"
  )
  implicit val decodeDecodeJson: DecodeJson[Decode] = DecodeJson(cursor =>
    (cursor --\ "character" --\ "href").as[Uri].map(Decode(_)))
  implicit val corporationDecodeJson: CodecJson[Character.Corporation] = casecodec3(Character.Corporation.apply, Character.Corporation.unapply)("id", "name", "href")
  implicit val characterDecodeJson: DecodeJson[Character] = casecodec2(Character.apply, Character.unapply)("id", "corporation")
  implicit val solarSystemJson: CodecJson[FleetMember.SolarSystem] =
    casecodec3(FleetMember.SolarSystem.apply, FleetMember.SolarSystem.unapply)("id", "name", "href")
  implicit val stationJson: CodecJson[FleetMember.Station] =
    casecodec3(FleetMember.Station.apply, FleetMember.Station.unapply)("id", "name", "href")
  implicit val shipJson: CodecJson[FleetMember.Ship] =
    casecodec3(FleetMember.Ship.apply, FleetMember.Ship.unapply)("id", "name", "href")
  implicit val fmCharacterJson: CodecJson[FleetMember.Character] =
    casecodec3(FleetMember.Character.apply, FleetMember.Character.unapply)("id", "name", "href")
  implicit val fleetMemberJson: CodecJson[FleetMember] =
    casecodec11(FleetMember.apply, FleetMember.unapply)("squadID", "solarSystem", "wingID", "roleID", "character", "boosterID", "boosterName", "roleName", "station", "ship", "joinTime")
  implicit val fleetMembersJson: CodecJson[FleetMembers] =
    casecodec1(FleetMembers.apply, FleetMembers.unapply)("items")
}
