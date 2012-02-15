package bytecode
import scala.annotation.target.beanGetter

class MyClazz(var name:String){
  
  def `x=`(v : String){
    name = v
  }
  
}