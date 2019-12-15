package eu.bossqone.aoc.solutions

import eu.bossqone.aoc.Day

object Day04 extends Day(4) {

  val parts = lines(0).split("-").map(_.toInt)
  val start = parts(0)
  val end = parts(1)

  val passwords =
    for (i <- start to end) yield i.toString.split("").map(_.toInt).toSeq

  def generateChunks(password: Seq[Int]): Seq[(Int, Int)] =
    password.sliding(2).toSeq.map(chunk => chunk.head -> chunk(1))

  def containsDouble(chunks: Seq[(Int, Int)]): Boolean = chunks.exists {
    case (first, second) => first == second
  }

  def containsUniqueDouble(chunks: Seq[(Int, Int)]) =
    chunks.groupBy(identity).view.mapValues(_.size).exists {
      case ((first, second), size) => first == second && size == 1
    }

  def neverDecreases(chunks: Seq[(Int, Int)]) = !chunks.exists {
    case (first, second) => first > second
  }

  override def solutionA = {
    passwords.count(password => {
      val chunks = generateChunks(password)
      containsDouble(chunks) && neverDecreases(chunks)
    })
  }

  override def solutionB = {
    passwords.count(password => {
      val chunks = generateChunks(password)
      containsUniqueDouble(chunks) && neverDecreases(chunks)
    })
  }
}
