package eu.bossqone.aoc.solutions

import eu.bossqone.aoc.Day

object Day03 extends Day(3) {

  case class Position(x: Int, y: Int) {

    private def move(orientation: Char, offset: Int): Position =
      orientation match {
        case 'R' => Position(x + offset, y)
        case 'L' => Position(x - offset, y)
        case 'D' => Position(x, y - offset)
        case 'U' => Position(x, y + offset)
      }

    def move(instruction: String): Seq[Position] = {
      val orientation = instruction(0)
      val step = instruction.substring(1).toInt

      for (offset <- 1 to step) yield move(orientation, offset)
    }

  }

  def calculatePath(
      instructions: Iterable[String]
  ): Seq[Position] =
    instructions
      .foldLeft((Seq.empty[Position], Position(0, 0))) {
        case ((path, currentPosition), instruction) =>
          val movedPath = currentPosition.move(instruction)
          val lastPosition = movedPath.last
          (path ++ movedPath, lastPosition)
      }
      ._1

  val wire1Instructions = lines(0).split(",")
  val wire2Instructions = lines(1).split(",")

  override def solutionA = {
    val wire1Path = calculatePath(wire1Instructions)
    val wire2Path = calculatePath(wire2Instructions)
    val crossPaths = wire1Path.intersect(wire2Path)

    crossPaths.map(position => Math.abs(position.x) + Math.abs(position.y)).min
  }

  override def solutionB = {
    val wire1Path = calculatePath(wire1Instructions)
    val wire2Path = calculatePath(wire2Instructions)
    val crossPaths = wire1Path.intersect(wire2Path)

    crossPaths
      .map(position => {
        val wire1Steps = wire1Path.indexOf(position) + 1
        val wire2Steps = wire2Path.indexOf(position) + 1

        wire1Steps + wire2Steps
      })
      .min
  }
}
