/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/19/13
 * Time: 5:48 PM
 * To change this template use File | Settings | File Templates.
 */
class Square {

  import Directions._
  import Characters._

  def square(size: Int): Element = {

    size match {
      case 0 => {
        Element.create("")
      }
      case 1 => {
        Element.create("+")
      }
      case 2 => {
        Element.create("++") above Element.create("++")
      }
      case _ => {

        var top: Element = start
        for (x <- 1 to size - 2) {
          top = top besideRight horizontalRight
        }
        top = top besideRight corner

        var left: Element = verticalUp
        for (x <- 1 to size - 3) {
          left = verticalUp above left
        }

        var right: Element = verticalDown
        for (x <- 1 to size - 3) {
          right = right above verticalDown
        }

        var bottom: Element = corner
        for (x <- 1 to size - 2) {
          bottom = bottom besideRight horizontalLeft
        }
        bottom = bottom besideRight corner

        val sq = (top above (left besideRight (right padLeftWith (top.width - 1)))) above bottom
        sq
      }

    }
  }

}
