import cats.implicits._

import org.atnos.eff.{Eff, |=}
import org.atnos.eff.all._

sealed trait Toy1[A]
final case class Doing1(v: String)                    extends Toy1[String]

object Toy1 {
  type _toy[R] = Toy1 |= R

  def doing[R  : _toy](v: String): Eff[R, String] =
    Eff.send(Doing1(v))
}
 
