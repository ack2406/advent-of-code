package com.ackmp.adventofcode.year2021

import scala.annotation.tailrec

object Day04 {

  def getNumbersOrder(input: List[String]): List[Int] = {
    input
      .head
      .split(",")
      .map(_.toInt).toList
  }

  def getBoards(input: List[String]): List[List[List[(Int, Boolean)]]] = {
    input
      .tail
      .filter(_ != "")
      .map(_.split(" ").toList.filter(_ != "").map(x => (x.toInt, false)))
      .grouped(5).toList
  }

  def checkBoard(board: List[List[(Int, Boolean)]]): Boolean = {
    board.map(_.count({ case (_, x) => x })).filter(_ == 5).sum > 0
  }

  def checkIfWinner(board: List[List[(Int, Boolean)]]): Boolean = {
    checkBoard(board) || checkBoard(board.transpose)
  }

  def markNumbers(board: List[List[(Int, Boolean)]], number: Int): List[List[(Int, Boolean)]] = {
    board
      .map(_.map({
        case (boardNumber, mark) => if (boardNumber == number) (boardNumber, true) else (boardNumber, mark)
      }))
  }

  def getIndexOfLastToWinBoard(boardState: List[Boolean], lastBoardState: List[Boolean]): Int = {
    boardState.zip(lastBoardState).indexWhere({ case (a, b) => a != b })
  }

  def calculateScore(board: List[List[(Int, Boolean)]], number: Int): Int = {
    board.flatMap(x => x.filter({ case (_, mark) => !mark})).map({ case (n, _) => n }).sum * number
  }

  @tailrec
  def playBingo(boards: List[List[List[(Int, Boolean)]]], numbers: List[Int], lastCall: Int = 0): Int = {
    if (boards.map(checkIfWinner).count(_ == true) > 0) {
      calculateScore(boards(boards.indexWhere(checkIfWinner)), lastCall)
    }
    else playBingo(boards.map(x => markNumbers(x, numbers.head)), numbers.tail, numbers.head)
  }

  @tailrec
  def playSquidBingo(boards: List[List[List[(Int, Boolean)]]], numbers: List[Int], lastCall: Int = 0, lastBoardState: List[Boolean] = List()): Int = {
    if (boards.map(checkIfWinner).count(_ == true) == boards.length) {
      calculateScore(boards(getIndexOfLastToWinBoard(boards.map(checkIfWinner), lastBoardState)), lastCall)
    }
    else playSquidBingo(boards.map(x => markNumbers(x, numbers.head)), numbers.tail, numbers.head, boards.map(checkIfWinner))
  }

  lazy val input: List[String] = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day04.txt")).getLines
    .toList

  def main(args: Array[String]): Unit = {
    println(playBingo(getBoards(input), getNumbersOrder(input)))
    println(playSquidBingo(getBoards(input), getNumbersOrder(input)))

  }
}