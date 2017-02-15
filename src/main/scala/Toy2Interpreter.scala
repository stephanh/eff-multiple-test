import cats.data.Writer
import cats.implicits._

import org.atnos.eff.{|=, Eff, Member, Translate}
import org.atnos.eff.Interpret.translate
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Toy2Interpreter {
  type Toy2Writer[A] = Writer[Toy2[Any], A]
  type _toy2Writer[R] = Toy2Writer |= R


  def runToy2[R, U, A](effects: Eff[R, A])(implicit m: Member.Aux[Toy2, R, U], writer: _toy2Writer[U]): Eff[U, A] =
    translate(effects)(new Translate[Toy2, U]{
      def apply[T](cmd: Toy2[T]): Eff[U, T] = cmd match {
        case Doing2(v) => {
          tell[U, Toy2[Any]](cmd.asInstanceOf[Toy2[Any]]) >>
          pure("OK")
        }
      }
    })
}
