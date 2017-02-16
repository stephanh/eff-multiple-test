import cats.data.Writer
import cats.implicits._

import org.atnos.eff.{|=, Eff, Member, Translate}
import org.atnos.eff.Interpret.translate
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Toy1Interpreter {
  type Toy1Writer[A] = Writer[Toy1[Any], A]
  type _toy1Writer[R] = Toy1Writer |= R


  def runToy1[R, U, A](effects: Eff[R, A])(implicit m: Member.Aux[Toy1, R, U], writer: _toy1Writer[U], option: _option[U]): Eff[U, A] =
    translate(effects)(new Translate[Toy1, U]{
      def apply[T](cmd: Toy1[T]): Eff[U, T] = cmd match {
        case Doing1(v) => {
          tell[U, Toy1[Any]](cmd.asInstanceOf[Toy1[Any]]) >>
          some("OK")
        }
      }
    })
}
