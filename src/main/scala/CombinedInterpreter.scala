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

  // Doesn't work
  type Stack1 = Fx.fx9[Toy1Writer, Toy2Writer, Toy3Writer, StateToy3, Toy1, Toy2, Toy3, WriterLog, Option]
  val eff1 = tell[Stack1, Log](Log("test"))

  def run1 = {
    eff1
      .runToy1
      .runWriterNoLog[Toy1[Any]]
      .runToy2
      .runWriterNoLog[Toy2[Any]]
      .runToy3
      .runWriterNoLog[Toy3[Any]]
      .evalState(ToyState(2))
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
      .runToy2
      .runWriterNoLog[Toy2[Any]]
      .runToy3
      .runWriterNoLog[Toy3[Any]]
      .evalState(ToyState(2))
      .runWriterNoLog[Log]
      .runOption
      .run
  }

  // Doesn't work
  type Stack3 = Fx.fx9[Toy1Writer, Toy2Writer, Toy3Writer, StateToy3, Toy1, Toy2, Toy3, WriterLog, Option]
  val eff3 = tell[Stack3, Log](Log("test"))

  def run3 = {
    eff3
      .runToy1
      .runToy2
      .runToy3
      .runOption
      .evalState(ToyState(2))
      .runWriterNoLog[Toy1[Any]]
      .runWriterNoLog[Toy2[Any]]
      .runWriterNoLog[Toy3[Any]]
      .runWriterNoLog[Log]
      .run
  }

  // Doesn't work
  type Stack4 = Fx.fx9[Toy1, Toy2, Toy3, Option, Toy1Writer, Toy2Writer, Toy3Writer, StateToy3, WriterLog]
  val eff4 = tell[Stack4, Log](Log("test"))

  def run4 = {
    eff4
      .runToy1
      .runToy2
      .runToy3
      .runWriterNoLog[Toy1[Any]]
      .runWriterNoLog[Toy2[Any]]
      .runWriterNoLog[Toy3[Any]]
      .runWriterNoLog[Log]
      .evalState(ToyState(2))
      .runOption
      .run
  }

  //type Stack5 = Fx.fx5[Toy1, Toy1Writer, StateToy3, WriterLog, Option] // Works
  type Stack5 = Fx.fx4[Option, Toy1, Toy1Writer, WriterLog] // Doesn't work
  val eff5 = tell[Stack5, Log](Log("test"))
  def run5 = {
    eff5
      .runToy1
      .runWriterNoLog[Toy1[Any]]
      .runWriterNoLog[Log]
      .runOption
      .run
  }

}
