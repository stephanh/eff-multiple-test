import cats.implicits._

import org.atnos.eff.{Eff, |=}
import org.atnos.eff.all._

sealed trait Toy2[A]
final case class Doing2(v: String)                    extends Toy2[String]

object Toy2 {
  type _toy[R] = Toy2 |= R

  def doing[R  : _toy](v: String): Eff[R, String] =
    Eff.send(Doing2(v))
}
 
