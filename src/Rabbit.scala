/**
 * Created with IntelliJ IDEA.
 * User: jdelouche
 * Date: 5/20/13
 * Time: 3:27 PM
 * To change this template use File | Settings | File Templates.
 */
class Rabbit {

  import Element._

  def rabbit = {
    var e =
      create("^   ^").padRightWith(15) ^
      create("||  ||").padRightWith(15) ^
      create("|---|| -----").padRightWith(15) ^
      create("|    |/     \\").padRightWith(15) ^
      create("|° ° |       |").padRightWith(15) ^
      create("| -  |       |").padRightWith(15) ^
      create("| O  |       |O").padRightWith(15) ^
      create("\\----/oo---oo").padRightWith(15)
    e
  }
}
