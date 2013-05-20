/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/19/13
 * Time: 5:41 PM
 * To change this template use File | Settings | File Templates.
 */
object Directions extends Enumeration {

  type Direction = Value
  val Right, Up, Left, Down = Value

  def next(current: Directions.Direction) = {
    Directions((current.id + 1) % Directions.maxId)
  }
}

