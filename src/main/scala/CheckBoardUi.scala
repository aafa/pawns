import Main.{Cell, CheckBoard}

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom
import scalatags.JsDom.all._

@JSExport
object CheckBoardUi extends JSApp{

  val boxSize = 10
  val cellsCount = boxSize * boxSize

  @JSExport
  def main(b: html.Body): Unit = {
    val visitor = RandomVisitor(CheckBoard(boardSize = boxSize))

    renderContent(b, visitor)
    dom.window.setInterval(() => {
      visitor.step()
      renderContent(b, visitor)
    }, 100)
  }

  private def renderContent(body: html.Body, visitor: RandomVisitor) = {
    body.innerHTML = ""
    type CellView   = JsDom.TypedTag[HTMLElement]

    def cell(string: JsDom.Modifier, cell: Cell) = {
      val cls = if (visitor.isVisited(cell)) "col pure-button pure-button-primary" else "col pure-button"
      div(`class` := cls, string, onclick := { () => visitor.startWalking(cell) })
    }

    val filler: IndexedSeq[CellView] =
        (0 until boxSize).flatMap { x =>
          (0 until boxSize).map { y =>
            cell(s"($x, $y)", (x, y))
          }
        }

    body.appendChild(
      div(
        `class` := "container",
        div(
          `class` := "grid-10",
          filler
        ),
        p("version: 2")
      ).render
    )
  }

  override def main(): Unit = {}
}
