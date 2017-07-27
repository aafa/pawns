import com.sun.tools.jdeps.Analyzer.Visitor

import scala.annotation.tailrec
import scala.collection.mutable

object Main {

  type Size = Int // assume square board
  type CellValue = Boolean // isVisited
  type Cell = (Size, Size) // cell coordinates (x,y)

  case class CheckBoard(boardSize: Size) {
    private val board = mutable.Map.empty[Cell, CellValue]
    private val cellsCount = boardSize * boardSize

    def visit(key: Cell): Unit = board.put(key, true)
    def allVisited: Boolean =
      board.values.size == cellsCount &&
        board.values.forall(_ == true)

    def percentageVisited: Float = board.values.count(_ == true).toFloat / cellsCount

    def isVisited(key: Cell): Boolean = board.getOrElse(key, false)

    def possibleMove(key: Cell): Seq[Cell] = {
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
        case (x, y) => x >= 0 && y >= 0 && x <= boardSize && y <= boardSize
      }
    }

    def bestNextMove(key: Cell): Option[Cell] =
      possibleMove(key).filterNot(isVisited).headOption // todo think of better strategy
  }

  class Visitor(val cb: CheckBoard) {
    def visitFrom(startingCell: Cell): Unit = {
      @tailrec def visitNext(key: Cell): Unit = {
        println(s"visitted $key")

        cb.visit(key)

        if (!cb.allVisited) {
          cb.bestNextMove(key) match {
            case Some(move) => visitNext(move)
            case None => print(s"not all visited, only ${cb.percentageVisited}")
          }


        }
      }

      visitNext(startingCell)

    }
  }
}
