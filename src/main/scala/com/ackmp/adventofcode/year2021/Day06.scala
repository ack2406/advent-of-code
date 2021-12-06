package com.ackmp.adventofcode.year2021

import scala.annotation.tailrec

object Day06 {
  @tailrec
  def doCycle(fishes: List[Int], days: Int): Any = days match {
    case 0 => fishes.length
    case n =>
      doCycle(
        fishes
          .map(timer => if (timer == 0) 6 else timer - 1)
          ++
          fishes
            .map(_ - 1)
            .filter(_ == -1)
            .map(_ => 8),
        n - 1
      )
  }

  lazy val input: List[Int] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day06.txt")).getLines
    .flatMap(_.split(",").map(_.toInt))
    .toList

  def main(args: Array[String]): Unit = {
    println(doCycle(input, 80))
  }
}