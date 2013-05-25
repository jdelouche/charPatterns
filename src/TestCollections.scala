/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/21/13
 * Time: 7:45 PM
 * To change this template use File | Settings | File Templates.
 */
object TestCollections extends App {

  println("START")

  val contents: Array[String] = Array("Hello", "world", "You")

  println(contents.mkString("\n"))

  val mySet = contents.toSet

  val v = Vector(1,2,3,10)
  val m = contents.map ( x => x.length).max

  println(">>")
  println(m)
  println("--")

  println(v.reduceLeft(_ + _))

}
