import cats.data.Writer
import cats.implicits._

import org.atnos.eff.{|=, Eff, Member, Translate}
import org.atnos.eff.Interpret.translate
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Toy3Interpreter {
  type Toy3Writer[A] = Writer[Toy3[Any], A]
  type _toy3Writer[R] = Toy3Writer |= R


  def runToy3[R, U, A](effects: Eff[R, A])(implicit m: Member.Aux[Toy3, R, U], writer: _toy3Writer[U]): Eff[U, A] =
    translate(effects)(new Translate[Toy3, U]{
      def apply[T](cmd: Toy3[T]): Eff[U, T] = cmd match {
        case Doing3(v) => {
          tell[U, Toy3[Any]](cmd.asInstanceOf[Toy3[Any]]) >>
          pure("OK")
        }
      }
    })
}
