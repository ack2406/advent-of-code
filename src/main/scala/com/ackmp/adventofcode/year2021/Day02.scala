package com.ackmp.adventofcode.year2021

object Day02 {
  def calculatePosition(input: List[List[String]]): Any = {
    input.foldLeft(List(0, 0))({
      case (List(horizontalPos, depthPos), List(operator, parameter)) => operator match {
        case "forward" => List(horizontalPos + parameter.toInt, depthPos)
        case "down" => List(horizontalPos, depthPos + parameter.toInt)
        case "up" => List(horizontalPos, depthPos - parameter.toInt)
      }
    }).product
  }

  def calculatePositionCorrectly(input: List[List[String]]): Any = {
    input.foldLeft(List(0, 0, 0))({
      case (List(horizontalPos, depthPos, aim), List(operator, parameter)) => operator match {
        case "forward" => List(horizontalPos + parameter.toInt, depthPos + (aim * parameter.toInt), aim)
        case "down" => List(horizontalPos, depthPos, aim + parameter.toInt)
        case "up" => List(horizontalPos, depthPos, aim - parameter.toInt)
      }
    }).init.product
  }

  lazy val input: List[List[String]] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day02.txt")).getLines
    .map(_.split(" ").toList)
    .toList

  def main(args: Array[String]): Unit = {
    println(calculatePosition(input))
    println(calculatePositionCorrectly(input))
  }
}