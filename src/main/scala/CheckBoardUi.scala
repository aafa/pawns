import Main.{Cell, CheckBoard, Size}

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom
import scalatags.JsDom.all.{s, _}

@JSExport
object CheckBoardUi extends JSApp {

  val boxSize = 10
  val cellsCount = boxSize * boxSize
  val visitor = new WarnsdorfVisitor(CheckBoard(boardSize = boxSize))

  var iterator: Int = 0

  @JSExport
  def main(b: html.Body): Unit = {
    renderContent(b, visitor)
  }

  def startWalking(body: html.Body): Unit = {
    stopWalking
    iterator = dom.window.setInterval(
      () => {
        val s: Option[(Size, Size)] = visitor.step()
        if (s.nonEmpty) {
          renderContent(body, visitor)
        } else {
          dom.window.alert(s"done in ${visitor.steps.size} steps")
          stopWalking
        }
      },
      50
    )
  }

  def stopWalking: Unit = {
    dom.window.clearInterval(iterator)
  }

  private def renderContent(body: html.Body, visitor: Visitor) = {
    body.innerHTML = ""
    type CellView = JsDom.TypedTag[HTMLElement]

    def cell(string: JsDom.Modifier, cell: Cell) = {
      val cls =
        if (visitor.isVisited(cell)) "col pure-button pure-button-primary"
        else "col pure-button"
      div(`class` := cls, string, onclick := { () =>
        visitor.startWalking(cell)
        startWalking(body)
      })
    }

    val filler: IndexedSeq[CellView] =
      (0 to boxSize).flatMap { x =>
        (0 to boxSize).map { y =>
          if (x > 0 && y > 0) {
            cell("", (x - 1, y - 1))
          } else {
            if (x > 0) {
              div(x - 1, `class` := "col")
            } else if (y > 0) {
              div(y - 1, `class` := "col")
            } else div("", `class` := "col")
          }
        }
      }

    body.appendChild(
      div(
        h4("Pawns!"),
        h6(i("Refresh anytime to start all over again")),
        `class` := "container",
        div(
          `class` := "grid-11",
          filler
        ),
        p("version: 4")
      ).render
    )
  }

  override def main(): Unit = {}
}
