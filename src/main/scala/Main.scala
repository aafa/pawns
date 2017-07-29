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

    def possibleMoves(key: Cell): CellNeighbours =
      board.getOrElse(key, Seq.empty)

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

class RailwayVisitor10(cb: CheckBoard) extends Visitor(cb) {
  private val path: Iterator[Cell] = Iterator
    .continually(
      List(
        (0, 0),
        (3, 0),
        (6, 0),
        (9, 0),
        (9, 3),
        (7, 1),
        (4, 1),
        (1, 1),
        (1, 4),
        (1, 7),
        (4, 7),
        (7, 7),
        (9, 5),
        (9, 2),
        (7, 0),
        (4, 0),
        (1, 0),
        (1, 3),
        (3, 1),
        (0, 1),
        (0, 4),
        (0, 7),
        (2, 5),
        (0, 3),
        (2, 1),
        (5, 1),
        (8, 1),
        (8, 4),
        (8, 7),
        (5, 7),
        (2, 7),
        (0, 5),
        (0, 2),
        (2, 0),
        (5, 0),
        (8, 0),
        (8, 3),
        (6, 1),
        (9, 1),
        (9, 4),
        (9, 7),
        (7, 5),
        (7, 2),
        (4, 2),
        (1, 2),
        (1, 5),
        (1, 8),
        (4, 8),
        (7, 8),
        (9, 6),
        (9, 9),
        (6, 9),
        (3, 9),
        (0, 9),
        (0, 6),
        (2, 4),
        (5, 4),
        (3, 2),
        (6, 2),
        (6, 5),
        (6, 8),
        (9, 8),
        (7, 6),
        (7, 9),
        (4, 9),
        (1, 9),
        (1, 6),
        (3, 4),
        (5, 2),
        (8, 2),
        (8, 5),
        (8, 8),
        (5, 8),
        (2, 8),
        (4, 6),
        (4, 3),
        (7, 3),
        (9, 1),
        (9, 4),
        (6, 4),
        (6, 7),
        (3, 7),
        (5, 5),
        (3, 3),
        (6, 3),
        (6, 6),
        (4, 4),
        (2, 2),
        (5, 2),
        (8, 2),
        (6, 0),
        (6, 3),
        (6, 0),
        (3, 0),
        (3, 3),
        (3, 6),
        (0, 6),
        (2, 4),
        (4, 2),
        (4, 5),
        (2, 3),
        (2, 6),
        (2, 9),
        (5, 9),
        (8, 9),
        (8, 6),
        (5, 6),
        (7, 4),
        (4, 4),
        (6, 2),
        (6, 5),
        (3, 5),
        (3, 8),
        (0, 8),
        (3, 8),
        (5, 6),
        (5, 3),
        (7, 1),
        (4, 1),
        (1, 1),
        (1, 4),
        (4, 4),
        (2, 2),
        (0, 0)
      ))
    .flatten

  override def nextMove(key: Cell): Option[Cell] = {
    require(cb.boardSize == 10)
    Option(path.dropWhile(_ == key).next())
  }
}
