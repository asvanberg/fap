package fap.crest

import fap.crest.interpreter.{CrestCache, DedupingInterpreter}
import fap.crest.model.Character
import fap.crest.model.Character.Corporation
import fap.model.{CharacterID, CorporationID}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.{Free, State, StateT, ~>}
import scala.Function.const

class DedupingInterpreterSpecification extends Specification with ScalaCheck {
  // Type aliases to help scalaz
  type Foo[A] = State[Int, A]
  type Bar[A] = StateT[Foo, CrestCache, A]

  "Deduping interpreter" should {
    "not ask for the current character more than once" in prop { (character: Character) =>
      val ask = Free.liftFC(SelectedCharacter)
      val prg = ask >>= const(ask) >>= const(ask) >>= const(ask)
      val interpreter: CrestOp ~> Bar = new DedupingInterpreter[Foo](trackingInterpreter(character))
      val called = Free.runFC(prg)(interpreter).run(CrestCache(None)).exec(0)
      called must_=== 1
    }

    "not ask the current character if already cached" in prop { (character: Character) =>
      val prg = Free.liftFC(SelectedCharacter)
      val interpreter: CrestOp ~> Bar = new DedupingInterpreter[Foo](trackingInterpreter(character))
      val called = Free.runFC(prg)(interpreter).run(CrestCache(Some(character))).exec(0)
      called must_=== 0
    }

    "ask underlying interpreter if not cached" in prop { (character: Character) =>
      val prg = Free.liftFC(SelectedCharacter)
      val interpreter: CrestOp ~> Bar = new DedupingInterpreter[Foo](trackingInterpreter(character))
      val asked = Free.runFC(prg)(interpreter).eval(CrestCache(Some(character))).eval(0)
      asked must_=== character
    }
  }

  def trackingInterpreter(character: Character): CrestOp ~> State[Int, ?] = new (CrestOp ~> State[Int, ?]) {
    override def apply[A](fa: CrestOp[A]): State[Int, A] = State {
      called =>
        fa match {
          case GetFleetMembers(fleetID) =>
            (called, Nil)
          case SelectedCharacter =>
            (called + 1, character)
        }
    }
  }

  val corporationGen = for {
    id <- Gen.choose(Long.MinValue, Long.MaxValue)
    name <- Gen.alphaStr
  } yield Corporation(CorporationID(id), name, org.http4s.Uri())

  val characterGen = for {
    id <- Gen.choose(Long.MinValue, Long.MaxValue)
    corporation <- corporationGen
  } yield Character(CharacterID(id), corporation)

  implicit val characterArbitrary = Arbitrary(characterGen)
}
