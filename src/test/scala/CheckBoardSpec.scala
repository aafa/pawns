import Main.{Cell, CheckBoard}
import org.scalatest.{FunSpec, MustMatchers}

class CheckBoardSpec extends FunSpec with MustMatchers {
  val cb = CheckBoard(boardSize = 10)

  it("checkerboard should work") {
    cb.boardSize must ===(10)
  }

  it("should know if all cells are visited") {
    val cb = CheckBoard(boardSize = 2)
    val v = new RandomVisitor(cb)

    val list = List[Cell]((0, 0), (0, 1), (1, 0))
    list.foreach(e => v.visit(e))

    v.allVisited must ===(false)
    v.visit((1, 1))
    v.allVisited must ===(true)
  }

  it("random visitor should do work") {
    val v = new RandomVisitor(cb)
    v.visitAll((0, 0))

    v.allVisited must ===(true)
  }

  it("WarnsdorfVisitor should do work") {
    val v = new WarnsdorfVisitor(cb)
    v.visitAll((0, 0))

    v.allVisited must ===(true)
  }

  it("WarnsdorfVisitor performance") {
    new WarnsdorfVisitor(cb) {
      visitAll((0, 0))
      allVisited must ===(true)

      println(steps.size)
      steps.size must be < (10 * cellsToVisit)
    }

    new WarnsdorfVisitor(cb) {
      visitAll((4, 8))
      allVisited must ===(true)

      println(steps.size)
      steps.size must be < (15 * cellsToVisit)
    }

    // complexity nearly linear, within ~O(15*n) for the worst case
  }

  it("should find closed-shape path") {
    new WarnsdorfVisitor(cb) {
      visitAll((0, 0))
      allVisited must ===(true)
      println(s"$steps; count:${steps.size}")

      steps.remove(0)
      println(s"$steps; count:${steps.size}")

      while (!allVisited) {
        step()
      }

      println(s"$steps; count:${steps.size}")
    }
  }

  it("Iterator drop while") {
    val circular = Iterator.continually(List(1, 2, 3, 4)).flatten

    circular.dropWhile(_ != 2).drop(1).next() must === (3)
    circular.dropWhile(_ != 4).drop(1).next() must === (1)
  }

  it("RailwayVisitor should work") {
    new RailwayVisitor10(cb) {
      startWalking((0,0))
      step().get must ===(3, 0)
      step().get must ===(6, 0)
      println(steps)
    }

    new RailwayVisitor10(cb) {
      startWalking((9,9))
      step().get must ===(6, 9)
      println(steps)
    }

    new RailwayVisitor10(cb) {
      visitAll((9, 9))
      allVisited must ===(true)

      println(steps)
    }
  }

}
