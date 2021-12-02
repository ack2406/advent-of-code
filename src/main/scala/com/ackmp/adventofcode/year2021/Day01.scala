package com.ackmp.adventofcode.year2021

object Day01 {
  def countWindowIncreases(input: List[Int], window: Int = 1): Int = {
    input.sliding(window).map(_.sum).toList.sliding(2).count{ case List(a, b) => a < b }
  }

  lazy val input: List[Int] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day01.txt")).getLines
    .map(_.toInt)
    .toList
  
  def main(args: Array[String]): Unit = {
    println(countWindowIncreases(input))
    println(countWindowIncreases(input, 3))
  }
}