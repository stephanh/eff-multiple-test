import cats.data.{Writer, State}
import cats.implicits._

import org.atnos.eff.{Eff, Fx, Member, |=, Translate}
import org.atnos.eff.all._
import org.atnos.eff.Interpret.translate
import org.atnos.eff.syntax.all._

import Toy1Interpreter.{Toy1Writer, _toy1Writer}
import Toy2Interpreter.{Toy2Writer, _toy2Writer}
import Toy3Interpreter.{Toy3Writer, _toy3Writer}

case class Log(s: String)

case class ToyState(i: Int)

object CombinedInterpreter {
  type WriterLog[A] = Writer[Log, A]
  type StateToy1[A] = State[Map[String, Any], A]
  type StateToy2[A] = State[Map[String, String], A]
  type StateToy3[A] = State[ToyState, A]


  implicit class Ops[R, A](effects: Eff[R, A]) {
    def runToy1[U](implicit
      m: Member.Aux[Toy1, R, U],
      writer: _toy1Writer[U]
    ): Eff[U, A] = Toy1Interpreter.runToy1(effects)

    def runToy2[U](implicit
      m: Member.Aux[Toy2, R, U],
      writer: _toy2Writer[U]
    ): Eff[U, A] = Toy2Interpreter.runToy2(effects)

    def runToy3[U](implicit
      m: Member.Aux[Toy3, R, U],
      writer: _toy3Writer[U]
    ): Eff[U, A] = Toy3Interpreter.runToy3(effects)

  }

  // Doesn't work scala.MatchError: 0 (of class java.lang.Integer)
  type Stack1 = Fx.fx4[Option, Toy1, Toy1Writer, WriterLog]
  val eff1 = tell[Stack1, Log](Log("test"))

  def run1 = {
    eff1
      .runToy1
      .runWriterNoLog[Toy1[Any]]
      .runWriterNoLog[Log]
      .runOption
      .run
  }

  //Works
  val eff2 = pure[Stack1, String]("")

  def run2 = {
    eff2
      .runToy1
      .runWriterNoLog[Toy1[Any]]
      .runWriterNoLog[Log]
      .runOption
      .run
  }

  // Works
  type Stack3 = Fx.fx4[Toy1, Toy1Writer, WriterLog, Option] // Put Option first
  val eff3 = tell[Stack3, Log](Log("test"))
  def run3 = {
    eff3
      .runToy1
      .runWriterNoLog[Toy1[Any]]
      .runWriterNoLog[Log]
      .runOption
      .run
  }

  // Works
  def run4 = {
    eff1
      .runToy1
      .runOption // Run option first
      .runWriterNoLog[Toy1[Any]]
      .runWriterNoLog[Log]
      .run
  }
}
