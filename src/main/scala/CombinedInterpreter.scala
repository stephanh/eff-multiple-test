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
      writer: _toy1Writer[U],
      option: _option[U]
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

  type Stack = Fx.fx9[Toy1Writer, Toy2Writer, Toy3Writer, StateToy3, Toy1, Toy2, Toy3, WriterLog, Option]
  def run[A](effects: Eff[Stack, A]): Option[A] = {
    effects
      .runToy1
      .runWriterNoLog[Toy1[Any]]
      .runToy2
      .runWriterNoLog[Toy2[Any]]
      .runToy3
      .runWriterNoLog[Toy3[Any]]
      .runOption
      .evalState(ToyState(2))
      .runWriterNoLog[Log]
      .run
  }
}
