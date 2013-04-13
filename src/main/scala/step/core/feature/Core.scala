package step.core.feature

import reflect.BeanProperty
import annotation.target.beanGetter

/**
 * User: sun-april
 * Date: 10-12-20
 */


object Core {

  // import all members under collection.mutable except BitSet

  import collection.mutable.{BitSet => _, _}


  /**
   * scala doc
   *
   *
   * @param ver specify document version @see java.lang.Object#toString
   *
   * @return document body
   *
   * @version 0.0.3
   * @since 2.8.0
   *
   * @author spp
   * @author SongPeng
   */
  def document(ver: Int, context: Map[String, String]): String = {
    ""
  }

  @deprecated("use `document(ver,context)` instead")
  def document(ver: Int): String = {
    ""
  }


  @serializable
  class ImSerializable


  @throws(classOf[java.io.FileNotFoundException])
  def throwException() = {
  }

  def iCallAMethodWithAnException = {
    throwException()
  }

  class BeanAnnotation(@beanGetter require: Boolean)

  def main(args: Array[String]) {

    val t = new BeanAnnotation(false)
  }

}