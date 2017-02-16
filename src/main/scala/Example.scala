import CombinedInterpreter.Stack

object Example {
  def main(args: Array[String]): Unit = {
    val x = for {
      _ <- Toy3.doing[Stack]("3a")
      _ <- Toy3.doing[Stack]("3b")
      _ <- Toy2.doing[Stack]("2a")
      _ <- Toy1.doing[Stack]("1a")
    } yield ()

    CombinedInterpreter.run(x)
  }
}
