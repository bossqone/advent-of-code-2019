package eu.bossqone.aoc.solutions

import eu.bossqone.aoc.Day

object Day01 extends Day(1) {

  val masses = lines.map(_.toInt)

  def calculateFuel(mass: Int): Int = mass / 3 - 2
  def calculateTotalFuel(fuel: Int): Int =
    if (fuel < 0) {
      0
    } else {
      fuel + calculateTotalFuel(calculateFuel(fuel))
    }

  override def solutionA = masses.map(calculateFuel).sum

  override def solutionB =
    masses
      .map(calculateFuel)
      .map(calculateTotalFuel)
      .sum

}
