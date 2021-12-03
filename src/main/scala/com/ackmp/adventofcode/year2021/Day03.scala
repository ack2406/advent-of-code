package com.ackmp.adventofcode.year2021

object Day03 {
  def getPowerConsumption(input: List[List[String]]): Any = {
    val gammaRate = input
      .transpose
      .map(_.count(_ == "1"))
      .map(x => if (x > input.length / 2) "1" else "0")
      .mkString("")

    val epsilonRate = gammaRate
      .split("")
      .map(x => if (x == "1") "0" else "1")
      .mkString("")

    Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2)
  }

  lazy val input: List[List[String]] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day03.txt")).getLines
    .map(_.split("").toList)
    .toList

  def main(args: Array[String]): Unit = {
    println(getPowerConsumption(input))
  }
}