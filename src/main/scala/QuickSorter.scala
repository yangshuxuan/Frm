import scala.reflect.ClassTag

/**
  * Created by yangshuxuan on 17-6-4.
  */
class QuickSorter[T:Ordering:ClassTag](val arr:Array[T]) {
  def quickSort() {
    def partition(start: Int, end: Int): Int = {

      var i = start
      var j = end
      var flag = true
      while (i < j) {
        if (implicitly[Ordering[T]].gt(arr(i), arr(j))) {
          val t = arr(i)
          arr(i) = arr(j)
          arr(j) = t
          flag = !flag
        }
        if(!flag){
          i += 1
        } else {
          j -= 1
        }
      }
      i
    }

    def quickSortHelp(start: Int, end: Int ): Unit = {
      if(start < end ) {
        val p = partition(start, end)
        //println(p)
        quickSortHelp(p + 1, end)
        quickSortHelp(start, p - 1)
      }

    }
    quickSortHelp(0,arr.size -1 )
  }
}
object QuickSorter{
  def main(args: Array[String]): Unit = {
    val t = Array(99,101,3,88,1,4,2,5,88,66,33,21)
    val q = new QuickSorter[Int](t)
    q.quickSort()
    val showMsg =t.mkString(",")
    println(showMsg)
  }
}