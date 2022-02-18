package com.ackmp.adventofcode.year2021

import scala.annotation.tailrec

object Day11 {
  var flashCounter = 0

  def increaseEnergy(octs: List[List[(Int, Boolean)]], i: Int, j: Int): List[List[(Int, Boolean)]] = {
    var newOcts = octs
    val newEnergy: (Int, Boolean) = newOcts(i)(j) match {
      case (9, false) =>
        flashCounter += 1
        val positions = for (
          posx <- i - 1 to i + 1;
          posy <- j - 1 to j + 1
          if !(posx == i && posy == j) &&
            posx >= 0 && posx < newOcts.length &&
            posy >= 0 && posy < newOcts.length)
        yield (posx, posy)
        newOcts = newOcts.updated(i, newOcts(i).updated(j, (10, true)))
        positions.foreach({ case (x, y) => newOcts = increaseEnergy(newOcts, x, y)})
        (10, true)
      case (energy, flashed) =>
        (energy + 1, flashed)
    }
    newOcts.updated(i, newOcts(i).updated(j, newEnergy))
  }

  def makeStep(octs: List[List[(Int, Boolean)]]): List[List[(Int, Boolean)]] = {

    var newOcts = octs
    for (i <- octs.indices) {
      for (j <- octs.head.indices) {
        newOcts = increaseEnergy(newOcts, i, j)
      }
    }
    newOcts.map(row => row.map({
      case (_, true) => (0, false)
      case x => x
    }))
  }

  @tailrec
  def simulate(input: List[List[(Int, Boolean)]], counter: Int): Any = {
    counter match {
      case 0 => input
      case _ => simulate(makeStep(input), counter - 1)
    }
  }

  @tailrec
  def findSync(input: List[List[(Int, Boolean)]], counter: Int = 0): Any = {
    input.flatten.map({ case (x, _) => x}).sum match {
      case 0 => counter
      case _ => findSync(makeStep(input), counter + 1)
    }
  }

  lazy val input: List[List[(Int, Boolean)]] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day11.txt")).getLines
    .map(row => row.split("").map(el => (el.toInt, false)).toList)
    .toList

  def main(args: Array[String]): Unit = {
    println(simulate(input, 100))
    println(flashCounter)
    println(findSync(input))
  }
}