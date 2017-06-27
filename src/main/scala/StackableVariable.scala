/**
  * Created by yangshuxuan on 17-6-27.
  */
package com.adups.frm


class StackableVariable[T](init:T) {
  private var values:List[T] = List(init)
  def value = values.head
  def withValue[R](newValue:T)(op: => R):R={
    values = newValue :: values
    try op finally values = values.tail
  }
}
class Signal[T](expr: => T){
  import Signal._
  private var myExpr: ()=>T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)
  protected def update(expr: => T): Unit ={
    myExpr = () => expr
    computeValue()
  }
  protected def computeValue():Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if(newValue!=myValue){
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())

    }
  }
  def apply()={
    observers += caller.value
    assert(!caller.value.observers.contains(this),"cyclic signal definition")
    myValue
  }

}
object NoSignal extends Signal[Nothing](???)
object Signal{
  private val caller = new StackableVariable[Signal[_]](NoSignal)

  def apply[T](expr: => T) = new Signal(expr)
}