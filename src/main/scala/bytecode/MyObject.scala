package bytecode

object MyObject {

  def main(args: Array[String]) {
    
    val c = new MyClazz("1")
    Console println c.name
    
    c.name = "2"
    Console println c.name
    
    c `x=` "hello"
    
  }

}