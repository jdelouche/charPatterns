import sun.reflect.annotation.ExceptionProxy

/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/19/13
 * Time: 5:48 PM
 * To change this template use File | Settings | File Templates.
 */
object Characters {
  import Element.create
  val corner = create("+")
  val start = create("+")
  val space = create(" ")
  val horizontalRight = create("--")
  val horizontal = create("-")
  val horizontalLeft = create("--")
  val verticalDown = create("|")
  val verticalUp = create("|")
  val vertical = create("|")
  val arrowDown = create("v")
  val arrowUp = create("^")
  val arrowRight = create(">")
  val arrowLeft = create("<")
  val upRight = create("/")
  val downLeft = upRight
  val downRight = create("\\")
  val upLeft = create("/")
}

