import java.io.File
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/16/13
 * Time: 1:33 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Element {

  import Element._

  val contents: List[String]
  val height = contents.length
  val width = if (height == 0) {
    0
  } else {
    contents.map {
      _.length
    }.max
  }

  override def toString = {
    this match {
      case Element.nothing => ""
      case _ => contents mkString "\n"
    }
  }

  def split(): List[Element] = {
    contents.map(create(_))
  }

  def concatenate(newContents: List[Element]): Element = {
    newContents.reduceLeft(_ above _)
  }

  def put(where: (Element, Element) => Element)(that: Element): Element = {

    (this, that) match {
      case (_, Element.nothing) => this
      case (Element.nothing, _) => that
      case (_, _) => where(that, this)
    }

  }

  def onTop(x: Element, y: Element): Element = {
    create(y.contents ++ x.contents)
  }

  def onBottom(x: Element, y: Element): Element = {
    create(x.contents ++ y.contents)
  }

  def onLeft(x: Element, y: Element): Element = {
    create(for ((line1, line2) <- x.contents zip y.contents) yield line1 + line2)
  }

  def onRight(x: Element, y: Element): Element = {
    create(for ((line1, line2) <- y.contents zip x.contents) yield line1 + line2)
  }

  def above(that: Element): Element = put(onTop)(that)

  def below(that: Element): Element = put(onBottom)(that)

  def besideLeft(that: Element): Element = put(onLeft)(that)

  def besideRight(that: Element): Element = put(onRight)(that)

  def vertical(e: Element, x: Element): Element = {
    e below x
  }

  def horizontal(e: Element, x: Element): Element = {
    e besideRight x
  }

  def multiplyOn(on: (Element, Element) => Element)(x: Int): Element = {
    x match {
      case 0 => {
        nothing
      }
      case _ => {
        var e: Element = nothing
        var i = 0
        for (i <- 1 to x) {
          e = on(this, e)
        }
        e
      }
    }
  }

  def hMultiplyBy(x: Int): Element = multiplyOn(horizontal)(x)

  def vMultiplyBy(x: Int): Element = multiplyOn(vertical)(x)

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

  def pad(size: Int): Element = {
    create(' ', size, 1)
  }

  def padOnTheRight(x: Element, w: Int): Element = {
    x besideRight pad(w - x.width)
  }

  def padOnTheLeft(x: Element, w: Int): Element = {
    x besideLeft pad(w - x.width)
  }

  def padSideOn(where: (Element, Int) => Element)(w: Int) = {

    val newContents = for {
      x <- split()
    } yield where(x, w)
    concatenate(newContents)

  }

  def padRightWith(w: Int) = padSideOn(padOnTheRight)(w)

  def padLeftWith(w: Int) = padSideOn(padOnTheLeft)(w)

  def aboveIt(x: Element, y: Element): Element = {
    x above y
  }

  def belowIt(x: Element, y: Element): Element = {
    x below y
  }

  def besideIt(x: Element, y: Element): Element = {
    x besideRight y
  }

  def padHorizontal(where: (Element, Element) => Element)(h: Int) = {
    if (h <= height) {
      this
    } else {
      val x = create(' ', width, h - height)
      where(this, x)
    }
  }

  def padTopWith(h: Int) = padHorizontal(belowIt)(h)

  def padBottomWith(h: Int) = padHorizontal(aboveIt)(h)

  def join(how: (Element, Element) => Element, where: (Element, Element) => Element)(line: String, e: Element) = {

    var mirroredLine: Element = nothing
    line.foreach {
      c: Char =>
        val s: String = c.toString
        val x = create(s)
        mirroredLine = how(x, mirroredLine)
    }
    where(mirroredLine, e)
  }


  def rotateClockwise45(x: Element, m: Element): Element = {
    x match {
      case Characters.horizontal => m v (Characters.space >> Characters.vertical)
      case Characters.horizontalLong => m v (Characters.vertical >> Characters.space)
      case Characters.vertical => m v (Characters.horizontal xh 2)
      case Characters.space => m v (Characters.space xh 2)
      case Characters.arrowUp => m v Characters.space >> Characters.arrowRight
      case Characters.corner => m v Characters.space >> Characters.corner
      case _ => {
        m v (Characters.space >> x)
      }
    }
  }


  def leftToRight(x: Element, y: Element): Element = {
    x match {
      case Characters.upRight => y << (Characters.downRight)
      case Characters.downRight => y << (Characters.upRight)
      case Characters.arrowRight => y << (Characters.arrowLeft)
      case Characters.arrowLeft => y << (Characters.arrowRight)
      case _ => y << x
    }
  }

  def upsideDown(x: Element, y: Element): Element = {
    x match {
      case Characters.upRight => y >> Characters.downRight
      case Characters.downRight => y >> Characters.upRight
      case Characters.arrowDown => y >> Characters.arrowUp
      case Characters.arrowUp => y >> Characters.arrowDown
      case _ => y >> x
    }
  }

  def mirror(into: (String, Element) => Element) = {
    var e: Element = nothing
    this.contents.foreach {
      line: String =>
        e = into(line, e)
    }
    e
  }

  def horizontal(line: String, e: Element) = join(upsideDown, aboveIt)(line, e)

  def vertical(line: String, e: Element) = join(leftToRight, belowIt)(line, e)

  def at90(line: String, e: Element) = join(rotateClockwise45, besideIt)(line, e)

  def mirrorH() = mirror(horizontal)

  def mirrorV() = mirror(vertical)

  def mirror45() = mirror(at90)

  def ^(that: Element): Element = above(that)


  def v(that: Element): Element = below(that)


  def >>(that: Element): Element = besideRight(that)


  def <<(that: Element): Element = besideLeft(that)


  def xh(x: Int): Element = hMultiplyBy(x)


  def xv(x: Int): Element = vMultiplyBy(x)


  def T(x: Element): Element = padTopWith(x)


  def B(x: Element): Element = padBottomWith(x)


  def R(x: Element): Element = padRightWith(x)


  def L(x: Element): Element = padLeftWith(x)


  def unary_~(): Element = this.mirrorH()


  def unary_!(): Element = this.mirrorV()

  def erase(): Element = {
    create('\010',width,height)
  }
}

object Element {

  class ArrayElement(val contents: List[String]) extends Element

  case class LineElement(s: String) extends ArrayElement(List(s))

  class UniformElement(ch: Char, w: Int, h: Int) extends ArrayElement(List.fill(h)(ch.toString * w))

  def create(contents: List[String]) = new ArrayElement(contents)

  def create(s: String) = new LineElement(s)

  def create(ch: Char, w: Int, h: Int) = new UniformElement(ch, w, h)

  def createFromFile(f: String) = {
    var elem: Element = nothing
    Source.fromFile(f).getLines().foreach{ line: String =>
      elem = create(line) below elem
    }
    elem.padRightWith(elem.width)
  }

  val nothing = create("")

}
