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

  type StackC0 = Fx.fx8[Toy1Writer, Toy2Writer, Toy3Writer, Toy1, Toy2, Toy3, WriterLog, Option]
  def runC0 = {
    val effC0 = for {
      _ <- Toy3.doing[StackC0]("3a")
      _ <- Toy3.doing[StackC0]("3b")
      _ <- Toy2.doing[StackC0]("2a")
     // _ <- Toy1.doing[Stack0]("1a")
    } yield ()

    effC0
      .runToy1
      .runWriterNoLog[Toy1[Any]]
      .runToy2
      .runWriterNoLog[Toy2[Any]]
      .runToy3
      .runWriterNoLog[Toy3[Any]]
      //.evalState(ToyState(2))
      .runWriterNoLog[Log]
      .runOption
      //.runOption
      .run
  }



  // Doesn't work scala.MatchError: 0 (of class java.lang.Integer)
  type StackM1 = Fx.fx4[Option, Toy1, Toy1Writer, WriterLog]
  val eff1 = tell[StackM1, Log](Log("test"))

  def run1 = {
    eff1
      .runToy1
      .runWriterNoLog[Toy1[Any]]
      .runWriterNoLog[Log]
      .runOption
      .run
  }

  //Works
  val eff2 = pure[StackM1, String]("")

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
