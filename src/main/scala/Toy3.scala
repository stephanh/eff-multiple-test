import cats.implicits._

import org.atnos.eff.{Eff, |=}
import org.atnos.eff.all._

sealed trait Toy3[A]
final case class Doing3(v: String)                    extends Toy3[String]

object Toy3 {
  type _toy[R] = Toy3 |= R

  def doing[R  : _toy](v: String): Eff[R, String] =
    Eff.send(Doing3(v))
}
 
