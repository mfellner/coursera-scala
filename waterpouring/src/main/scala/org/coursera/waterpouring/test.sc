import org.coursera.waterpouring.Pouring

object test {
  val problem = new Pouring(Vector(4, 9))
  problem.moves.mkString("\n")

  problem.solutions(6)
}
