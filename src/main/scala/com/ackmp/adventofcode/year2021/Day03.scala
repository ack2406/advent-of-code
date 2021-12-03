package com.ackmp.adventofcode.year2021

object Day03 {
  // part 1

  def toDec(str: String): Int = Integer.parseInt(str, 2)

  def getGammaRate(input: List[List[String]]): String = input
      .transpose
      .map(_.count(_ == "1"))
      .map(x => if (x >= input.length.toFloat / 2) "1" else "0")
      .mkString("")
    
  def getEpsilonRate(input: List[List[String]]): String = getGammaRate(input)
      .split("")
      .map(x => if (x == "1") "0" else "1")
      .mkString("")

  def getPowerConsumption(input: List[List[String]]): Any = {
    toDec(getGammaRate(input)) * toDec(getEpsilonRate(input))
  }

  // part 2
  def getLifeSupportRating(input: List[List[String]]): Any = {
    def getRating(input: List[List[String]], n: Int, rateType: (List[List[String]]) => String,rates: List[String] = List()): String = input match {
      case x if x.length == 1 => x.head.mkString
      case _ => {
        val newRates = rateType(input).split("").toList
        getRating(input.filter(x => x(x.length - n) == newRates(newRates.length - n)), n - 1, rateType, newRates)
      }
    }
    toDec(getRating(input, input.head.length, getGammaRate)) * toDec(getRating(input, input.head.length, getEpsilonRate))
  }

  lazy val input: List[List[String]] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day03.txt")).getLines
    .map(_.split("").toList)
    .toList

  def main(args: Array[String]): Unit = {
    println(getPowerConsumption(input))
    println(getLifeSupportRating(input))
  }
}