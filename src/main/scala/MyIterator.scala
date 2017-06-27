import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
  * Created by yangshuxuan on 17-6-11.
  */
trait MyIterator[T] {
  import MyIterator._
  def hasNext():Boolean
  def next():T
  def map[B:ClassTag](f:T => B): MyIterator[B] ={
    val buf = new ArrayBuffer[B]()
    while(hasNext()){
      buf += f(next())
    }
    buildFactory(buf.toArray)
  }
}
object MyIterator{
  def apply[T:ClassTag](arr:T*): MyIterator[T] ={
    buildFactory(arr.toArray)

  }
  def buildFactory[T](arr:Array[T]): MyIterator[T] ={
    new MyIterator[T] {
      val maxLen = arr.length

      var indicator = -1

      override def next(): T = {
        indicator += 1
        arr(indicator)
      }

      override def hasNext(): Boolean = if(indicator < maxLen -1) true else false
    }
  }

  def main(args: Array[String]): Unit = {
    val myIterator = MyIterator(1,2,3).map(x => math.sqrt(x))
    while(myIterator.hasNext())
      println(myIterator.next())
  }
}
