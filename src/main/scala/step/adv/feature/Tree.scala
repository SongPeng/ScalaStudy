package step.adv.feature

/**
 * Created by IntelliJ IDEA.
 * User: sun-april
 * Date: 10-12-20
 * Time: 上午3:45
 * To change this template use File | Settings | File Templates.
 */

object Tree {

  abstract class BinaryTree

  case object EmptyTree extends BinaryTree

  case class Node(elem: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree

  def main(args: Array[String]) {

  }


}