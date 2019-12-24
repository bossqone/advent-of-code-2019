package eu.bossqone.aoc.solutions

import eu.bossqone.aoc.Day

import scala.io.StdIn

object Day05 extends Day(5) {

  val initialMemory = lines(0).split(",").map(_.toInt)

  def evaluateParam(memory: Seq[Int], value: Int, mode: Option[Char]) =
    mode match {
      case Some('1') => memory(value)
      case _         => memory(memory(value))
    }

  @scala.annotation.tailrec
  def compute(memory: Seq[Int], ip: Int = 0): Int = {
    val opcodeWithModes = memory(ip).toString
    val opcode = opcodeWithModes.takeRight(2).toInt
    val modes = opcodeWithModes.dropRight(2).reverse

    opcode match {
      case 1 =>
        val x = evaluateParam(memory, ip + 1, modes.lift(0))
        val y = evaluateParam(memory, ip + 2, modes.lift(1))
        val out = memory(ip + 3)
        val value = x + y
        compute(memory.updated(out, value), ip + 4)

      case 2 =>
        val x = evaluateParam(memory, ip + 1, modes.lift(0))
        val y = evaluateParam(memory, ip + 2, modes.lift(1))
        val out = memory(ip + 3)
        val value = x * y
        compute(memory.updated(out, value), ip + 4)

      case 3 =>
        val value = StdIn.readInt()
        val out = memory(ip + 1)
        compute(memory.updated(out, value), ip + 2)

      case 4 =>
        val value = evaluateParam(memory, ip + 1, modes.lift(0))
        println(value)
        compute(memory, ip + 2)

      case 5 =>
        val cond = evaluateParam(memory, ip + 1, modes.lift(0))
        val newIp = if (cond > 0) {
          evaluateParam(memory, ip + 2, modes.lift(1))
        } else {
          ip + 3
        }
        compute(memory, newIp)

      case 6 =>
        val cond = evaluateParam(memory, ip + 1, modes.lift(0))
        val newIp = if (cond == 0) {
          evaluateParam(memory, ip + 2, modes.lift(1))
        } else {
          ip + 3
        }
        compute(memory, newIp)

      case 7 =>
        val x = evaluateParam(memory, ip + 1, modes.lift(0))
        val y = evaluateParam(memory, ip + 2, modes.lift(1))
        val out = memory(ip + 3)
        val result = if (x < y) {
          1
        } else {
          0
        }
        compute(memory.updated(out, result), ip + 4)

      case 8 =>
        val x = evaluateParam(memory, ip + 1, modes.lift(0))
        val y = evaluateParam(memory, ip + 2, modes.lift(1))
        val out = memory(ip + 3)
        val result = if (x == y) {
          1
        } else {
          0
        }
        compute(memory.updated(out, result), ip + 4)

      case 99 => memory(0)
    }
  }

  override def solutionA = compute(initialMemory)

  override def solutionB = compute(initialMemory)
}
