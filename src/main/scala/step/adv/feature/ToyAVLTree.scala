/**
 *
 */
package step.adv.feature

/**
 * @author songpengpeng
 *
 */
object AVLTree {

  /**
   * 2011-04-03
   * toy-writing for fun and studying scala
   * base on scala collections framework
   * @see scala.collection.immutable.RedBlack
   */

  trait Tree[A, +B] {
    def insert[B1 >: B](kv: (A, B1)): Tree[A, B1]
    def search(k: A): Option[B]
    def delete(k: A): Tree[A, B]
  }

  abstract class BinarySearch[K] {
    def isSmaller(k1: K, k2: K): Boolean

    abstract class Tree[+V] {
      def isEmpty: Boolean
      def insert[B >: V](key: K, value: B): Tree[B]
      def search(key: K): Tree[V]
      def delete(key: K): Tree[V]
    } // end Tree

    case object Empty extends Tree[Nothing] {
      def isEmpty = true

      def search(k: K): Tree[Nothing] = this

      def insert[B](key: K, value: B): Tree[B] =
        Node[B](key, value, Empty, Empty)

      def delete(key: K): Tree[Nothing] = this
    } // end Empty

    case class Node[+V](key: K, val value: V, left: Tree[V], right: Tree[V])
      extends Tree[V] {

      def isEmpty = false

      def delete(k: K): Tree[V] = {
        if (isSmaller(k, key))
          Node(key, value, left.delete(k), right)
        else if (isSmaller(key, k))
          Node(key, value, left, right.delete(k))
        else {
          del(k)
        }
      }

      private def del[B >: V](k: K): Tree[B] = {
        if (left.isEmpty && right.isEmpty)
          return Empty

        if (left.isEmpty)
          return right

        if (right.isEmpty)
          return left

        var (newSubTree, replacing) =
          adjustSubTree(left.asInstanceOf[Node[V]])

        Node(replacing.key, replacing.value, newSubTree, right)
      }

      private def adjustSubTree[B >: V](node: Node[B]): (Tree[B], Node[B]) = {
        var replacingNode: Node[B] = node
        
        def construction(node: Node[B]): Tree[B] = {
          if (node.left.isEmpty && node.right.isEmpty) {
            replacingNode = node
            Empty
          } else if (node.right.isEmpty) {
            replacingNode = node
            node.left
          } else {
            Node(node.key, node.value, node.left,
              construction(node.right.asInstanceOf[Node[B]]))
          }
        }

        (construction(node), replacingNode)
      }

      def insert[B >: V](k: K, v: B): Tree[B] = {
        if (isSmaller(key, k))
          Node(key, value, left, right.insert(k, v))
        else if (isSmaller(k, key))
          Node(key, value, left.insert(k, v), right)
        else
          Node(key, v, left, right)
      }

      def search(k: K): Tree[V] = {
        if (isSmaller(key, k))
          right search k
        else if (isSmaller(k, key))
          left search k
        else 
          this
      }
    } // end class Node

  } // end BinarySearch

  class UnImplementedError extends Error

  trait AVLTreeLike[A, +B] extends Tree[A, B] {
    def rotate()
  }

  object AVLTree {
  	
    def make[A, B](tree: BinarySearch[A]#Tree[B],
    		size: Int)(implicit ord: Ordering[A]): AVLTree[A, B] = {
    	
      new AVLTree[A, B](size, tree)(ord)
    }
    
    def empty[A, B](implicit ord: Ordering[A]): AVLTree[A, B] =
      make[A, B](null, 0)
      
  } // end object AVLTree

  class AVLTree[A, +B](val size: Int, 
  		t: BinarySearch[A]#Tree[B])(implicit val ord: Ordering[A])
    extends BinarySearch[A]
    with AVLTreeLike[A, B] {

    def isSmaller(k1: A, k2: A) = {
      ord.lt(k1, k2)
    }

    private val tree: BinarySearch[A]#Tree[B] =
      if (size == 0) Empty else t

    @throws(classOf[UnImplementedError])
    def rotate() = throw new UnImplementedError

    def insert[B1 >: B](kv: (A, B1)): AVLTree[A, B1] = {
      val newsize =
        if (tree.search(kv._1).isEmpty) size + 1 else size
        
      AVLTree.make(tree.insert(kv._1, kv._2), newsize)(ord)
    }

    def delete(key: A): AVLTree[A, B] = {
      if (tree.search(key).isEmpty)
        this
      else 
        AVLTree.make(tree.delete(key), size - 1)
    }

    def search(k: A): Option[B] = {
      tree.search(k) match {
        case n: Node[B] => Some(n.value)
        case _ => None
      }
    }

    override def toString(): String = tree.toString

  } // end class AVLTree

  def main(arr: Array[String]): Unit = {
    case class Person(id: String, name: String)
    var tree = AVLTree.empty[Int, Person]
    tree = tree.insert(10, Person("10-1", "god"))
    tree = tree.insert(5, Person("5-1", "dog"))
    tree = tree.insert(15, Person("15-1", "cat"))
    tree = tree.insert(2, Person("2-1", "cat"))
    tree = tree.insert(7, Person("7-1", "cat"))
    tree = tree.insert(6, Person("6-1", "6"))

    val p = tree.search(6)
    Console println p

    tree = tree.delete(10)

    Console println tree
  } // end main method

} // end object AVLTree