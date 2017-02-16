import CombinedInterpreter.Initial

object Example {
  def main(args: Array[String]): Unit = {
    val x = for {
      _ <- Toy3.doing[Initial]("3a")
      _ <- Toy3.doing[Initial]("3b")
      _ <- Toy2.doing[Initial]("2a")
      _ <- Toy1.doing[Initial]("1a")
    } yield ()

    val (a, cmds) = CombinedInterpreter.run(x)
    println(cmds.mkString("\n"))
  }
}
