package fap.crest

import _root_.argonaut.DecodeJson
import fap.crest.model._
import fap.model.{CharacterID, FleetID}
import org.http4s._
import org.http4s.argonaut.jsonOf
import org.http4s.client.Client
import org.http4s.dsl._
import org.http4s.headers.Authorization

import scalaz.concurrent.Task
import scalaz.syntax.monad._
import scalaz.{Kleisli, _}

object interpreter {

  type CrestAPI[A] = CrestResponseT[Task, A]

  private implicit def jsonEntityDecoder[A: DecodeJson]: EntityDecoder[A] = jsonOf[A]

  def apply(client: Client, server: Server) = new (CrestOp ~> Kleisli[CrestAPI, OAuth2BearerToken, ?]) {
    def crestCall[A: EntityDecoder](request: Request): CrestResponseT[Task, A] = {
      CrestResponseT(client.fetch(request) {
        case Ok(response) => response.as[A].map(CrestResponse.Ok(_))
        case Unauthorized(_) => Task.now(CrestResponse.Unauthorized)
        case Forbidden(_) => Task.now(CrestResponse.Forbidden)
        case _ => Task.now(CrestResponse.Error)
      })
    }
    override def apply[A](fa: CrestOp[A]): Kleisli[CrestAPI, OAuth2BearerToken, A] =
      Kleisli.kleisli {
        token =>
          fa match {
            case GetFleetMembers(CharacterID(characterID), FleetID(fleetID)) =>
              val req = Request(
                uri = server.root / "fleets" / fleetID.toString / "members" / "",
                headers = Headers(Authorization(token))
              )
              client.fetch(req) {
                case Ok(response) => response.as[FleetMembers].map(a => CrestResponse.Ok(a.members))
                case Unauthorized(_) => Task.now(CrestResponse.Unauthorized)
                case Forbidden(_) => Task.now(CrestResponse.Forbidden)
                case _ => Task.now(CrestResponse.Error)
              }
              crestCall[FleetMembers](req).map(_.members)
            case SelectedCharacter =>
              val req = Request(
                uri = server.root / "decode" / "",
                headers = Headers(Authorization(token))
              )
              for {
                characterLocation <- crestCall[Decode](req)
                character <- crestCall[Character](Request(uri = characterLocation.href))
              } yield character.id
          }
      }
  }

  sealed trait CrestResponse[+A]
  object CrestResponse {
    implicit val crestResponseInstances: Traverse[CrestResponse] with Monad[CrestResponse] = new Traverse[CrestResponse] with Monad[CrestResponse] {
      override def traverseImpl[G[_], A, B](fa: CrestResponse[A])(f: (A) => G[B])(implicit F: Applicative[G]): G[CrestResponse[B]] =
        fa match {
          case Ok(a) => F.map(f(a))(Ok(_))
          case x => F.point(x.asInstanceOf[CrestResponse[B]])
        }

      override def bind[A, B](fa: CrestResponse[A])(f: (A) => CrestResponse[B]): CrestResponse[B] = fa match {
        case Ok(a) => f(a)
        case x => x.asInstanceOf[CrestResponse[B]]
      }

      override def point[A](a: => A): CrestResponse[A] = Ok(a)
    }
    final case class Ok[A](a: A) extends CrestResponse[A]
    case object Unauthorized extends CrestResponse[Nothing]
    case object Forbidden extends CrestResponse[Nothing]
    case object Error extends CrestResponse[Nothing]
  }

  final case class CrestResponseT[F[_], A](run: F[CrestResponse[A]]) {
    def map[B](f: A => B)(implicit F: Functor[F]): CrestResponseT[F, B] =
      CrestResponseT(run.map {
        case CrestResponse.Ok(a) => CrestResponse.Ok(f(a))
        case x => x.asInstanceOf[CrestResponse[B]]
      })

    def flatMap[B](f: A => CrestResponseT[F, B])(implicit F: Monad[F]): CrestResponseT[F, B] =
      CrestResponseT(Monad[F].bind(run) {
        case CrestResponse.Ok(a) => f(a).run
        case x => Monad[F].point(x.asInstanceOf[CrestResponse[B]])
      })
  }
  object CrestResponseT {
    implicit def crestResponseTMonad[F[_]: Monad]: Monad[CrestResponseT[F, ?]] = new Monad[CrestResponseT[F, ?]] {
      override def map[A, B](fa: CrestResponseT[F, A])(f: A => B) = fa map f

      override def bind[A, B](fa: CrestResponseT[F, A])(f: A => CrestResponseT[F, B]): CrestResponseT[F, B] =
        fa flatMap f

      override def point[A](a: => A): CrestResponseT[F, A] =
        CrestResponseT(Monad[F].point(CrestResponse.Ok(a)))
    }
  }
}