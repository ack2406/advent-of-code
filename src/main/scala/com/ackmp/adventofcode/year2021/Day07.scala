package com.ackmp.adventofcode.year2021

object Day07 {
  def getOptimalPosition(crabs: List[Int]): Int = {
    (crabs.min to crabs.max)
    .map(pos => crabs.map(crab => (crab - pos).abs).sum)
    .min
  }

  def getCorrectPosition(crabs: List[Int]): Int = {
    (crabs.min to crabs.max)
    .map(pos =>
      crabs.map(crab =>
        (crab - pos).abs * ((crab - pos).abs + 1) / 2).sum)
        .min
  }

  lazy val input: List[Int] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day07.txt")).getLines
    .flatMap(_.split(",").map(_.toInt))
    .toList

  def main(args: Array[String]): Unit = {
    println(getOptimalPosition(input))
    println(getCorrectPosition(input))
  }
}