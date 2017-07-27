import Main.{Cell, CheckBoard, Visitor}
import org.scalatest.{FunSpec, MustMatchers}

class CheckBoardSpec extends FunSpec with MustMatchers {
  val cb = CheckBoard(boardSize = 10.toByte)

  it("checkerboard should work") {
    cb.boardSize must ===(10)
  }

  it("should know if all cells are visited") {
    val cb = CheckBoard(boardSize = 2.toByte)
    val list = List[Cell]((0, 0), (0, 1), (1, 0))
    list.foreach(e => cb.visit(e))

    cb.allVisited must ===(false)
    cb.visit((1, 1))
    cb.allVisited must ===(true)
  }

  it("visitor should do work") {
    val v = new Visitor(cb)
    v.visitFrom((0, 0))
  }
}
