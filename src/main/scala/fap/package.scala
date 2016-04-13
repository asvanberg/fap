import java.time.Instant

import argonaut.CodecJson
import fap.crest.CrestOp
import fap.fleet.FleetOp
import org.http4s.OAuth2BearerToken
import org.http4s.client.Client

import scalaz.{Coproduct, Kleisli}
import scalaz.concurrent.Task

package object fap {
  type Fap[A] = Coproduct[CrestOp, FleetOp, A]
  type FapTask[A] = Kleisli[Task, (Client, OAuth2BearerToken), A]

  implicit val instantJsonCodec: CodecJson[Instant] = CodecJson.derived[Long].xmap(Instant.ofEpochMilli)(_.toEpochMilli)
}
