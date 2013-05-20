/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/16/13
 * Time: 1:33 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Element {

  import Element._

  val contents: Array[String]
  val height = contents.length
  val width = if (height == 0) 0 else contents(0).length

  override def toString = {
    contents mkString "\n"
  }

  def above(that: Element): Element = {
    create(this.contents ++ that.contents)
  }

  def below(that: Element): Element = {
    create(that.contents ++ this.contents)
  }

  def besideRight(that: Element): Element = {
    create(for ((line1, line2) <- this.contents zip that.contents) yield line1 + line2)
  }

  def besideLeft(that: Element): Element = {
    create(for ((line1, line2) <- that.contents zip this.contents) yield line1 + line2)
  }

  def hMultiplyBy(x: Int): Element = {
    var i = 0
    x match {
      case 0 => {
        nothing
      }
      case _ => {
        var e: Element = this
        var i = 0
        for (i <- 2 to x) {
          e = e besideRight this
        }
        e

      }
    }
  }

  def vMultiplyBy(x: Int): Element = {
    var i = 0
    x match {
      case 0 => {
        nothing
      }
      case _ => {
        var e: Element = this
        var i = 0
        for (i <- 2 to x) {
          e = e below this
        }
        e
      }
    }
  }

  def ^(that: Element): Element = {
    above(that)
  }

  def v(that: Element): Element = {
    below(that)
  }

  def >>(that: Element): Element = {
    besideRight(that)
  }

  def <<(that: Element): Element = {
    besideLeft(that)
  }

  def xh(x: Int): Element = {
    hMultiplyBy(x)
  }

  def xv(x: Int): Element = {
    vMultiplyBy(x)
  }

  def T(x: Element): Element = {
    padTopWith(x)
  }

  def B(x: Element): Element = {
    padBottomWith(x)
  }

  def R(x: Element): Element = {
    padRightWith(x)
  }

  def L(x: Element): Element = {
    padLeftWith(x)
  }

  def padRightWith(that: Element): Element = {
    padRightWith(that.width)
  }

  def padBottomWith(that: Element): Element = {
    padBottomWith(that.height)
  }

  def padLeftWith(that: Element): Element = {
    padLeftWith(that.width)
  }

  def padTopWith(that: Element): Element = {
    padTopWith(that.height)
  }

  def padRightWith(w: Int) = {
    if (w <= width) {
      this
    } else {
      val right = create(' ', w - width, height)
      this besideRight right
    }
  }

  def padBottomWith(h: Int) = {
    if (h <= height) {
      this
    } else {
      val bot = create(' ', width, h - height)
      this above bot
    }
  }

  def padLeftWith(w: Int) = {
    if (w <= width) {
      this
    } else {
      val left = create(' ', w - width, height)
      left besideRight this
    }
  }

  def padTopWith(h: Int) = {
    if (h <= height) {
      this
    } else {
      val top = create(' ', width, h - height)
      top above this
    }
  }

  def mirrorH(): Element = {
    var e: Element = nothing
    this.contents.foreach {
      line: String =>
        var mirroredLine: Element = nothing
        line.foreach {
          c: Char =>
            val s = c.toString
            val x = create(s)
            x match {
              case Characters.upRight => mirroredLine >>= Characters.downRight
              case Characters.downRight => mirroredLine >>= Characters.upRight
              case Characters.arrowDown => mirroredLine >>= Characters.arrowUp
              case Characters.arrowUp => mirroredLine >>= Characters.arrowDown
              case _ => mirroredLine >>= x
            }
        }
        e = e v mirroredLine
    }
    e
  }

  def M():Element = {
    this.mirrorH()
  }

}

object Element {

  class ArrayElement(val contents: Array[String]) extends Element

  case class LineElement(s: String) extends ArrayElement(Array(s))

  class UniformElement(ch: Char, w: Int, h: Int) extends ArrayElement(Array.fill(h)(ch.toString * w))

  def create(contents: Array[String]) = new ArrayElement(contents)

  def create(s: String) = new LineElement(s)

  def create(ch: Char, w: Int, h: Int) = new UniformElement(ch, w, h)

  val nothing = create("")

}
