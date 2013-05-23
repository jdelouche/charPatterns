/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/16/13
 * Time: 1:50 PM
 * To change this template use File | Settings | File Templates.
 */

import Element._

object Test extends App {

  import Directions._

  val spir = new Spiral().spiral(10, Up)
  val squa = new Square().square(4)

  var e = (spir xv 2) xh 3
  println(e)
  println((((((squa B spir) >> spir >> (squa T spir)) >> spir >> (squa B spir)) xv 2) xh 3) v e)

  val ur = new Diagonal().diagonal(4, Up, Right)
  val dl = new Diagonal().diagonal(4, Down, Left)
  val ul = new Diagonal().diagonal(4, Up, Left)
  val dr = new Diagonal().diagonal(4, Down, Right)
  println(((ur >> dr) xh 8))
  println(((ur >> dr) xh 8).mirrorH())

  println(spir ^ ~spir)

  val rabbit = new Rabbit().rabbit
  val frame = new Square().square(8)
  println(frame >> rabbit >> frame)
  val line = rabbit >> rabbit.mirrorV()
  val enil = ~line
  println((rabbit >> ~rabbit) ^ !(rabbit >> ~rabbit))

}
