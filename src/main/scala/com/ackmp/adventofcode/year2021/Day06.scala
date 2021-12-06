package com.ackmp.adventofcode.year2021

import scala.annotation.tailrec

object Day06 {
  @tailrec
  def doCycle(fishes: List[Int], days: Int): Int = days match {
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

  def createTimers(fishes: List[Int]): List[Long] = {
    fishes.foldLeft(List.fill(9)(0): List[Long])((acc, t) => acc.updated(t, acc(t) + 1))
  }

  def fastCycle(timers: List[Long], days: Int): Long = {
    if (days == 0) timers.sum
    else timers match {
      case List(t0, t1, t2, t3, t4, t5, t6, t7, t8) =>
        fastCycle(List(t1, t2, t3, t4, t5, t6, t7 + t0, t8, t0), days - 1)
    }
  }

  lazy val input: List[Int] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day06.txt")).getLines
    .flatMap(_.split(",").map(_.toInt))
    .toList

  def main(args: Array[String]): Unit = {
    println(doCycle(input, 80))
    println(fastCycle(createTimers(input), 256))
  }
}