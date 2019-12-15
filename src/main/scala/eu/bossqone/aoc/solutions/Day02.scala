package eu.bossqone.aoc.solutions

import eu.bossqone.aoc.Day

object Day02 extends Day(2) {

  val initialMemory = lines(0).split(",").map(_.toInt)

  @scala.annotation.tailrec
  def compute(memory: Seq[Int], ip: Int = 0): Int = {
    val opcode = memory(ip)

    opcode match {
      case 1 =>
        val xAddress = memory(ip + 1)
        val yAddress = memory(ip + 2)
        val outAddress = memory(ip + 3)
        val result = memory(xAddress) + memory(yAddress)
        compute(memory.updated(outAddress, result), ip + 4)

      case 2 =>
        val xAddress = memory(ip + 1)
        val yAddress = memory(ip + 2)
        val outAddress = memory(ip + 3)
        val result = memory(xAddress) * memory(yAddress)
        compute(memory.updated(outAddress, result), ip + 4)

      case 99 => memory.head
    }
  }

  override def solutionA = compute(initialMemory.updated(1, 12).updated(2, 2))

  override def solutionB = {
    val attempts = for {
      noun <- 0 to 99
      verb <- 0 to 99
    } yield {
      val result = compute(initialMemory.updated(1, noun).updated(2, verb))

      if (result == 19690720) {
        Some(100 * noun + verb)
      } else {
        None
      }
    }

    attempts.flatten.head
  }
}
