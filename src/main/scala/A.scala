/**
  * Created by yangshuxuan on 17-6-10.
  */
class A  extends Iterator[Int]{
  var mylength = 0
  var maxLimt = 5



  override def hasNext: Boolean = if(mylength < maxLimt) true else false

  override def next(): Int = {
    mylength += 1
    5
  }


}
object A {
  def main(args: Array[String]): Unit = {
    val a = new A
    //a.foreach(println)
    val b: Iterator[Int] = a.map(x => x*x)
    b.foreach(println)

  }
}
