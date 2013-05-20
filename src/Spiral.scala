/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/19/13
 * Time: 5:24 PM
 * To change this template use File | Settings | File Templates.
 */
class Spiral {

  import Directions._
  import Characters._

  def spiral(nbEdges: Int, direction: Direction): Element = {

    if (nbEdges == 1) {
      start
    } else {
      val sp = spiral(nbEdges - 1, Directions.next(direction))
      var length = nbEdges - 1
      def goRight(length: Int): Element = {
        var line: Element = corner
        var x = 0
        for (x <- 1 to length) {
          line = line besideRight horizontalRight
        }
        line above (sp padRightWith line)
      }
      def goUp(length: Int): Element = {
        var column: Element = corner
        var x = 0
        for (x <- 1 to length) {
          column = verticalUp above column
        }
        column besideRight (sp padTopWith column)
      }
      def goLeft(length: Int): Element = {
        var line: Element = corner
        var x = 0
        for (x <- 1 to length) {
          line = horizontalLeft besideRight line
        }
        (sp padLeftWith line) above line
      }
      def goDown(length: Int): Element = {
        var column: Element = corner
        var x = 0
        for (x <- 1 to length) {
          column = column above verticalDown
        }
        (sp padBottomWith column) besideRight column
      }
      direction match {
        case Right => {
          goRight(length)
        }
        case Up => {
          goUp(length)
        }
        case Left => {
          goLeft(length)
        }
        case Down => {
          goDown(length)
        }
        case _ => {
          sp
        }
      }
    }
  }
}
