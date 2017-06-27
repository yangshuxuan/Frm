/**
  * Created by yangshuxuan on 17-6-12.
  */
class P
object P1 extends P
case class P2(v:Int) extends P
object B {
  def main(args: Array[String]): Unit = {
    val f = Traversable(P1,P2(3),P1)
    f.collect({ case P2(v) => v * v}).foreach(println)
    val b = Iterable(1,2,3,4)

  }

}
