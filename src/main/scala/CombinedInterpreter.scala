import cats.data.Writer
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

object CombinedInterpreter {
  type CmdWriter[A] = Writer[Cmd, A]

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

  type Stack = Fx.fx7[Toy1, Toy1Writer, Toy2, Toy2Writer, Toy3, Toy3Writer, CmdWriter]
  def run[A](effects: Eff[Stack, A]): (A, List[Cmd]) = {
    effects
      .runToy1
      .runWriterToy1
      .runToy3
      .runWriterToy3
      .runToy2
      .runWriterToy2
      .runWriter[Cmd]
      .run
  }
}
