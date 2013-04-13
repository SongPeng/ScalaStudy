package step.adv.feature

/**
 * User: sun-april
 * Date: 10-12-20
 */

object Tree {

  abstract class BinaryTree

  case object EmptyTree extends BinaryTree

  case class Node(elem: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree

  def main(args: Array[String]) {

  }


}