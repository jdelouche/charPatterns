/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/19/13
 * Time: 5:48 PM
 * To change this template use File | Settings | File Templates.
 */
class Diagonal {

  import Characters._
  import Directions._
  import Element._

  def diagonal(size: Int, x: Direction, y: Direction): Element = {

    size match {
      case 0 => {
        create("")
      }
      case 1 => {
        create("+")
      }
      case _ => {

        (x, y) match {
          case (Up, Right) | (Down, Left) => {
            var bottom: Element = upRight >> (space xh size - 1)
            var x = 0
            for (x <- 1 to size - 2) {
              bottom = bottom v ((space xh x) >> upRight >> (space xh (size-1-x)))
            }
            bottom = bottom v ((space xh (size - 1)) >> upRight)
            bottom
          }
          case (Down, Right) | (Up, Left) => {
            var top: Element = downRight >> (space xh size - 1)
            var x = 0
            for (x <- 1 to size - 2) {
              top = top ^ ((space xh x) >> downRight >> (space xh (size-1-x)))
            }
            top = top ^ ((space xh (size - 1)) >> downRight)
            top
          }
        }
      }
    }
  }
}
