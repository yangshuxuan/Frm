/**
  * Created by yangshuxuan on 17-6-16.
  */
package com.adups
abstract class Tree[+T] extends Traversable[T]{
  def element:T
  def left:Tree[T]
  def right:Tree[T]
  def treeSearch[U >: T :Ordering](k:U):Option[U]
  def treeInsert[U >: T :Ordering](k:U):Tree[U]= this match {
    case EmptyTree => new Branch[U](k,EmptyTree,EmptyTree)
    case Branch(e,l,r) => if(implicitly[Ordering[U]].equiv(k,e)) new Branch[U](k,l,r)
    else {
      if(implicitly[Ordering[U]].lt(k,e)) new Branch[U](e,l.treeInsert(k),r) else new Branch[U](e,l,r.treeInsert(k))

    }

  }
  def treeMinum:Option[T]=this match {
    case Branch(e,l,r) => l match {
      case EmptyTree => Some(e)
      case _ => l.treeMinum
    }
    case EmptyTree => None
  }
  def treeMaximum:Option[T]=this match {
    case Branch(e,l,r) => r match {
      case EmptyTree => Some(e)
      case _ => r.treeMinum
    }
    case EmptyTree => None
  }


  override def foreach[U](f: (T) => U): Unit = this match {
    case Branch(e,l,r) => {
      l.foreach(f)
      f(e)
      r.foreach(f)
    }
    case _ =>
  }
}
object EmptyTree extends Tree[Nothing]{
  override def element: Nothing = throw new NoSuchElementException("EmptyTree.element")

  override def left: Tree[Nothing] = throw new NoSuchElementException("EmptyTree.left")

  override def right: Tree[Nothing] = throw new NoSuchElementException("EmptyTree.right")

  override def treeSearch[U >: Nothing : Ordering](k: U): Option[U] = None
}
case class Branch[+T:Ordering](element:T,left:Tree[T],right:Tree[T]) extends Tree[T]{
  override def treeSearch[U >: T :Ordering](k: U): Option[U] =
    if(implicitly[Ordering[U]].equiv(k,element)) Some(element)
    else if(implicitly[Ordering[U]].lt(k,element)) left.treeSearch(k)
    else right.treeSearch(k)
}
object Tree{
  def main(args: Array[String]): Unit = {
    val e = new Branch[Int](5,EmptyTree,EmptyTree)
    val f = new Branch[Int](7,EmptyTree,EmptyTree)
    val g = new Branch[Int](6,e,f)
    val h = g.treeInsert(8).treeInsert(4)
    h.foreach(println)
    g.foreach(println)
    //g.treeSearch(7).foreach(println)
    //g.treeMaximum.foreach(println)
  }
}
