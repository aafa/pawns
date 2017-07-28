import Main.{Cell, CheckBoard}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object Main {

  type Size = Int // assume square board
  type CellValue = Boolean // cell state; isVisited
  type Cell = (Size, Size) // cell coordinates (x,y)

  case class CheckBoard(boardSize: Size) {
    private val board = mutable.Map.empty[Cell, CellValue]
    private val cellsCount = boardSize * boardSize

    def visit(key: Cell): Unit = board.put(key, true)
    def allVisited: Boolean =
      board.values.size == cellsCount &&
        board.values.forall(_ == true)

    def percentageVisited: Float =
      board.values.count(_ == true).toFloat / cellsCount

    def isVisited(key: Cell): Boolean = board.getOrElse(key, false)

    def possibleMoves(key: Cell): Seq[Cell] = {
      val moves: Seq[Cell] = key match {
        // starting from N - going CW
        case (x, y) =>
          Seq[Cell](
            (x, y - 3), // N
            (x + 2, y - 2),
            (x + 3, y), // E
            (x + 2, y - 2),
            (x, y + 3), // S
            (x - 2, y - 2),
            (x - 3, y), // W
            (x - 2, y - 2)
          )
      }

      moves.filter {
        case (x, y) => x >= 0 && y >= 0 && x < boardSize && y < boardSize
      }
    }

  }
}

abstract class Visitor(cb: CheckBoard) {
  var stepsCounter = 0
  var steps = mutable.ArrayBuffer.empty[Cell]

  def startWalking(cell: Cell): Unit = {
    steps = mutable.ArrayBuffer(cell)
  }

  def isVisited(cell: Cell): Boolean = steps.contains(cell)

  def step(): Option[Cell] = {
    cb.allVisited match {
      case false if steps.nonEmpty => nextMove(steps.last).map(c => {
        steps.append(c)
        c
      })
      case _ => None
    }
  }

  def visitAll(startingCell: Cell): Unit = {
    @tailrec def visitNext(key: Cell): Unit = {
      println(s"visited $key")

      cb.visit(key)

      if (!cb.allVisited) {
        nextMove(key) match {
          case Some(move) => visitNext(move)
          case None       => print(s"not all visited, only ${cb.percentageVisited}")
        }
      }
    }

    visitNext(startingCell)

  }

  def nextMove(key: Cell): Option[Cell]
}

// todo think of better strategy
case class RandomVisitor(cb: CheckBoard) extends Visitor(cb) {
  def nextMove(key: Cell): Option[Cell] =
    Random
      .shuffle(cb.possibleMoves(key))
      .lastOption
}
