/**
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Created with IntelliJ IDEA.                                         *
 * User: jdelouche                                                     *
 * Date: 5/16/13                                                       *
 * Time: 1:50 PM                                                       *
 * To change this template use File | Settings | File Templates.       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

import java.io.File

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
  val zigzag = ((ur >> dr) xh 8) ^ ((ur >> dr) xh 8).mirrorH()
  println(zigzag)

  println(spir ^ ~spir)

  val rabbit = new Rabbit().rabbit
  val frame = new Square().square(8)
  println(frame >> rabbit >> frame)
  val line = rabbit >> rabbit.mirrorV()
  val enil = ~line
  println((rabbit >> ~rabbit) ^ !(rabbit >> ~rabbit))
  println(squa)
  println(squa.mirror45())
  val fourRabbits = (rabbit.mirror45 B (rabbit ^ ~rabbit)) >> ((rabbit ^ ~rabbit) B rabbit.mirror45) >> (!rabbit.mirror45 B (rabbit ^ ~rabbit))
  println(fourRabbits)
  println(spir.mirror45())
  val zigzagVertical = zigzag.mirror45()
  println(zigzag.mirror45())
  println(zigzagVertical.mirror45() v
    (zigzagVertical >>
      fourRabbits >>
      ~fourRabbits >>
      (zigzagVertical.padLeftWith(zigzagVertical.mirror45.width - zigzagVertical.width - 2 * fourRabbits.width))) v
    zigzagVertical.mirror45())
  val times = 3
  //println(rabbit xh times)
  //println(rabbit xv times)
  val vbar = create(' ', rabbit.width, rabbit.height)
  println(rabbit >> rabbit.erase >> vbar)
  println(~rabbit)

  println(createFromFile("Rabbit.txt"))

  def createBigString(s: String): Element = {
    var ch: Element = nothing
    s.foreach {
      letter: Char =>
        letter match {
          case ' ' => {
            ch = ch >> createFromFile("space.txt")
          }
          case x => {
            def fileName(x: Char): String = {
              if (x.isLetter) {
                if (x.isLower) {
                  x + "Lower.txt"
                } else {
                  x + ".txt"
                }
              } else {
                x + ".txt"
              }
            }
            ch = ch >> createFromFile(fileName(x))
          }
        }
    }

    ch
  }

  private val Pierre: Element = createBigString("PIERRE").mirrorV().mirror45()
  private val Jean: Element = createBigString("JEAN")
  private val delouche: Element = createBigString("DELOUCHE")
  println(Jean.padBottomWith(Pierre) >> Pierre >> delouche.padTopWith(Pierre))
  private val g: Element = createBigString("g")
  println(createBigString("ABCDEFGHIJKLMNOPQRSTUVWX"))
  println(createBigString("abcdefghijklmnopqrstuvwx"))
  println(~createBigString("n"))
}
