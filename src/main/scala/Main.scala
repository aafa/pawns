import Main.{Cell, CheckBoard, Size}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object Main {

  type Size = Int // assume square board
  type Cell = (Size, Size) // cell coordinates (x,y)
  type CellNeighbours = Seq[Cell] // adjacent cells

  case class CheckBoard(boardSize: Size) {
    private val board: Map[Cell, CellNeighbours] =
      Seq
        .tabulate(boardSize, boardSize) {
          case (x, y) => (x, y) -> nextMoves((x, y))
        }
        .flatten
        .toMap[Cell, CellNeighbours]

    def possibleMoves(key: Cell): CellNeighbours = board.getOrElse(key, Seq.empty)

    private def nextMoves(key: Cell): Seq[Cell] = {
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
  val cellsToVisit = cb.boardSize * cb.boardSize
  var steps = mutable.ArrayBuffer.empty[Cell]

  def startWalking(cell: Cell): Unit = {
    steps = mutable.ArrayBuffer(cell)
  }

  def isVisited(cell: Cell): Boolean = steps.contains(cell)

  def allVisited: Boolean = steps.toSet.size == cellsToVisit

  def percentageVisited: Float =
    steps.toSet.size.toFloat / cellsToVisit

  def step(): Option[Cell] = {
    allVisited match {
      case false if steps.nonEmpty =>
        nextMove(steps.last).map(c => {
          steps.append(c)
          c
        })
      case _ => None
    }
  }

  def visit(key: Cell): Unit = steps.append(key)

  def visitAll(startingCell: Cell): Unit = {
    @tailrec def visitNext(key: Cell): Unit = {
      println(s"visited $key")

      visit(key)

      if (!allVisited) {
        step() match {
          case Some(move) => visitNext(move)
          case None       => print(s"not all visited, only $percentageVisited")
        }
      }
    }

    startWalking(startingCell)
    visitNext(startingCell)

  }

  def nextMove(key: Cell): Option[Cell]
}

// todo think of better strategy
class RandomVisitor(cb: CheckBoard) extends Visitor(cb) {
  def nextMove(key: Cell): Option[Cell] =
    Random
      .shuffle(cb.possibleMoves(key))
      .lastOption
}

class WarnsdorfVisitor(cb: CheckBoard) extends RandomVisitor(cb) {
  override def nextMove(key: Cell): Option[Cell] = {
    val cells = cb.possibleMoves(key).filterNot(isVisited)
    if (cells.isEmpty) {
      super.nextMove(key)
    } else {
      Option(cells.minBy(cell => cb.possibleMoves(cell).size))
    }
  }
}
