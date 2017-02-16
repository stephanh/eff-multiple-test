import cats.data.{Writer, State}
import cats.implicits._

import org.atnos.eff.{Eff, Fx, Member, |=, Translate}
import org.atnos.eff.all._
import org.atnos.eff.Interpret.translate
import org.atnos.eff.syntax.all._

import Toy1Interpreter.{Toy1Writer, _toy1Writer}
import Toy2Interpreter.{Toy2Writer, _toy2Writer}
import Toy3Interpreter.{Toy3Writer, _toy3Writer}

sealed trait Cmd
case class Toy1Cmd(c: Toy1[Any]) extends Cmd
case class Toy2Cmd(c: Toy2[Any]) extends Cmd
case class Toy3Cmd(c: Toy3[Any]) extends Cmd

case class Log(s: String)

case class ToyState(i: Int)

object CombinedInterpreter {
  type CmdWriter[A] = Writer[Cmd, A]

  type WriterLog[A] = Writer[Log, A]
  type StateToy1[A] = State[Map[String, Any], A]
  type StateToy2[A] = State[Map[String, String], A]
  type StateToy3[A] = State[ToyState, A]


  implicit class Ops[R, A](effects: Eff[R, A]) {
    def runWriterToy1[U](implicit
      m: Member.Aux[Toy1Writer, R, U],
      writer: CmdWriter |= U
    ): Eff[U, A] = translate(effects)(new Translate[Toy1Writer, U] {
      def apply[X](w: Toy1Writer[X]): Eff[U, X] = {
        val (cmd, v) = w.run
        tell[U, Cmd](Toy1Cmd(cmd)).as(v)
      }
    })

    def runWriterToy2[U](implicit
      m: Member.Aux[Toy2Writer, R, U],
      writer: CmdWriter |= U
    ): Eff[U, A] = translate(effects)(new Translate[Toy2Writer, U] {
      def apply[X](w: Toy2Writer[X]): Eff[U, X] = {
        val (cmd, v) = w.run
        tell[U, Cmd](Toy2Cmd(cmd)).as(v)
      }
    })

    def runWriterToy3[U](implicit
      m: Member.Aux[Toy3Writer, R, U],
      writer: CmdWriter |= U
    ): Eff[U, A] = translate(effects)(new Translate[Toy3Writer, U] {
      def apply[X](w: Toy3Writer[X]): Eff[U, X] = {
        println(w)
        val (cmd, v) = w.run
        tell[U, Cmd](Toy3Cmd(cmd)).as(v)
      }
    })

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

  type Stack = Fx.fx12[CmdWriter, Toy1Writer, Toy2Writer, Toy3Writer, StateToy1, StateToy2, StateToy3, Toy1, Toy2, Toy3, WriterLog, Option]
  def run[A](effects: Eff[Stack, A]): (Option[A], List[Cmd]) = {
    effects
      .runToy1
      .runWriterToy1
      .runToy2
      .runWriterToy2
      .runToy3
      .runWriterToy3
      .runOption
      .evalState(Map.empty[String, Any])
      .evalState(Map.empty[String, String])
      .evalState(ToyState(2))
      .runWriter[Cmd]
      .runWriterNoLog[Log]
      .run
  }
}
