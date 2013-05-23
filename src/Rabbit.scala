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
    var e: Element =
      create("^   ^") ^
      create("||  ||") ^
      create("|---|| -----") ^
      create("|    |/     \\") ^
      create("|° ° |       |") ^
      create("| ^  |       |") ^
      create("| O  |       |O") ^
      create("\\----/oo---oo")
    e.padRightWith(15)
  }
}
