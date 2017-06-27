/**
  * Created by yangshuxuan on 17-6-24.
  */
trait Parsers {
  type Input = List[Char]
  abstract class ParserResult[T]
  case class Success[T](result:T,input:Input) extends ParserResult[T]
  trait Parser[T] extends (Input => ParserResult[T])

}
object Parsers{
  def main(args: Array[String]): Unit = {
    val p = """"""".r

  }
}