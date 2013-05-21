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

//  val sp = new Spiral().spiral(10, Up)
//  val sq = new Square().square(4)
//  //  println(sq)
//  val star = Element.create("*")
//  var e = (sp xv 2) xh 3
//  //  println(e)
//    println((((((sq B sp) >> sp >> (sq T sp)) >> sp >> (sq B sp)) xv 2) xh 3) v e)
//  //  println(sp besideRight sq besideRight sp)
//  //  println(sq besideRight sp besideRight sq)
//  val ur = new Diagonal().diagonal(4, Up, Right)
//  val dl = new Diagonal().diagonal(4, Down, Left)
//  val ul = new Diagonal().diagonal(4, Up, Left)
//  val dr = new Diagonal().diagonal(4, Down, Right)
//
//  //println(((ur >> dr) xh 4))
//  //println(((ur >> dr) xh 4).mirrorH())
//
//  //println(sp ^ ~sp)

  val rabbit = new Rabbit().rabbit
  //println(rabbit ^ ~rabbit)
  val frame = new Square().square(8)
  val line = rabbit >> rabbit.mirrorV()
  val enil = ~line
  println((rabbit >> ~rabbit) ^ !(rabbit >> ~rabbit))

}
