package fap

import doobie.syntax.catchable.ToDoobieCatchableOps._
import fap.crest._
import fap.model.{CharacterID, FleetID, FleetMember}
import org.http4s.argonaut._
import org.http4s.{Headers, OAuth2BearerToken, Request}
import org.http4s.client.Client
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.headers.Authorization

import scalaz.concurrent.Task
import scalaz.{Free, Kleisli, ~>}

object util {
  type Token = String




}
