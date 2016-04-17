import java.time.Instant

import argonaut.CodecJson
import fap.crest.CrestOp
import fap.crest.interpreter.CrestAPI
import fap.fleet.FleetOp
import org.http4s.OAuth2BearerToken

import scalaz.{Coproduct, Kleisli}

package object fap {
  type Fap[A] = Coproduct[CrestOp, FleetOp, A]
  type FapTask[A] = Kleisli[CrestAPI, OAuth2BearerToken, A]

  implicit val instantJsonCodec: CodecJson[Instant] = CodecJson.derived[Long].xmap(Instant.ofEpochMilli)(_.toEpochMilli)
}
