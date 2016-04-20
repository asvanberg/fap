package fap

import doobie.contrib.specs2.analysisspec.AnalysisSpec
import doobie.util.transactor.DriverManagerTransactor
import fap.model.{CharacterID, CorporationID, FleetID}
import org.flywaydb.core.Flyway
import org.specs2.mutable.Specification

import scalaz.concurrent.Task

object FleetQueriesSpecification extends Specification with AnalysisSpec {
  private val databaseUrl = "jdbc:h2:mem:fap;MODE=PostgreSQL;DB_CLOSE_DELAY=-1"

  override val transactor = DriverManagerTransactor[Task]("org.h2.Driver", databaseUrl)

  val flyway = new Flyway()
  flyway.setDataSource(databaseUrl, "", "")
  flyway.migrate()

  check(fleet.interpreter.queries.fleetMembers(FleetID(0)))
  check(fleet.interpreter.queries.insertFleet(FleetID(0), null, CharacterID(0), null, None))
  check(fleet.interpreter.queries.myFleets(CharacterID(0)))
  check(fleet.interpreter.queries.myParticipations(CharacterID(0)))
  check(fleet.interpreter.queries.corporationFleets(CorporationID(0)))
  check(fleet.interpreter.queries.fleetById(FleetID(0)))
}
