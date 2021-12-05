package com.ackmp.adventofcode.year2021

object Day05 {
  def getLines(input: List[String]): List[List[List[Int]]] = {
    input.map(_.split(" -> ").map(_.split(",").map(_.toInt).toList).toList)
  }

  def getPointsFromLine(line: List[List[Int]]): List[List[List[Int]]] = {
    line.transpose.map({
      case List(a, b) if b > a  => (a to b).toList
      case List(a, b)           => (b to a).toList.reverse
    }) match {
      case List(points, List(n)) => List(points.map(x => List(n, x)))
      case List(List(n), points) => List(points.map(x => List(x, n)))
      case List(points1, points2) => List(points2.lazyZip(points1) map (List(_, _))) // part 2 change
    }
  }

  def getLinesOfVents(input: List[String]): List[List[Int]] = {
    getLines(input)
      .flatMap(getPointsFromLine)
      .flatten
  }

  def getDiagramSize(lines: List[List[Int]]): (Int, Int) = {
    lines
      .foldLeft((0, 0)) ({
        case ((acc1, acc2), List(a, b)) => (List(acc1, a).max, List(acc2, b).max)
      })
  }

  def tabulaRasa(lines: List[List[Int]]): List[List[Int]] = {
    List.fill(getDiagramSize(lines)._1 + 1)(List.fill(getDiagramSize(lines)._2 + 1)(0))
  }

  def createDiagram(lines: List[List[Int]]): List[List[Int]] = {
    lines
      .foldLeft(tabulaRasa(lines))({
        case (acc, List(a, b)) => acc.updated(a, acc(a).updated(b, acc(a)(b) + 1))
      })
  }

  def countOverlappingPoints(input: List[String]): Int = {
    createDiagram(getLinesOfVents(input))
      .flatten
      .count(_ > 1)
  }

  lazy val input: List[String] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day05.txt")).getLines
    .toList

  def main(args: Array[String]): Unit = {
    println(countOverlappingPoints(input))
  }
}