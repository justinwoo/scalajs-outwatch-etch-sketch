import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.dom.raw._
import outwatch.dom._
import rxscalajs._

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

sealed trait Action
final case class MoveCursor(direction: Direction) extends Action
case object NoOp extends Action

final case class Coords(
  x: Int,
  y: Int
)

final case class State(
  cursor: Coords,
  points: Set[Coords],
  width: Int,
  height: Int,
  increment: Int
)

object Scalajsetchsketch extends JSApp {

  def isInvalidPoint(state: State, coords: Coords): Boolean = (coords, state) match {
    case (Coords(x, y), State(_, _, width, height, increment)) =>
      x < 0 || (increment * x) > (width - increment) ||
      y < 0 || (increment * y) > (height - increment)
  }

  def eval(state: State, action: Action): State = action match {
    case MoveCursor(direction) =>
      evalDirection(state, direction)
    case _ =>
      state
  }

  def shiftCursor(coords: Coords, direction: Direction): Coords = coords match {
    case Coords(x, y) =>
      direction match {
        case Up => Coords(x, y - 1)
        case Down => Coords(x, y + 1)
        case Left => Coords(x - 1, y)
        case Right => Coords(x + 1, y)
      }
  }

  def evalDirection(state: State, direction: Direction): State = {
    val cursor = shiftCursor(state.cursor, direction)
    if (isInvalidPoint(state, cursor)) {
      state
    } else {
      state.copy(cursor = cursor, points = state.points + state.cursor)
    }
  }

  def point(increment: Int, color: String, coords: Coords) = coords match {
    case Coords(x, y) =>
      div(
        style :=
          "position: absolute; left: " +
          x * increment +
          "px; top: " +
          y * increment +
          "px; width: " +
          increment +
          "px; height: " +
          increment +
          "px; background-color: " +
          color +
          ";"
      )
  }

  val initialState = State(
    Coords(0, 0),
    Set(),
    800,
    600,
    10
  )

  def main(): Unit = {
    val inputs = Observable.fromEvent(document.body, "keydown")
      .flatMap({
        case ke: KeyboardEvent =>
          Observable.just(ke.keyCode)
        case _ =>
          console.error("this should be literally impossible")
          Observable.just()
      })
      .flatMap({
        case 38 => Observable.just(MoveCursor(Up))
        case 40 => Observable.just(MoveCursor(Down))
        case 37 => Observable.just(MoveCursor(Left))
        case 39 => Observable.just(MoveCursor(Right))
        case _ => Observable.just()
      })

    val state = inputs
      .merge(Observable.just(NoOp))
      .scan(initialState)(eval)

    val view = state.map({
      case State(cursor, points, width, height, increment) =>
        div(
          h1("Hello!!!"),
          div(
            style :=
              "position: relative; width: " +
              width +
              "px; height: " +
              height +
              "px; border: 1px solid black;",
            span(points.map(point(increment, "black", _)).toSeq: _*),
            point(increment, "grey", cursor)
          )
        )
    })

    OutWatch.render("#app", div(child <-- view))
  }
}
