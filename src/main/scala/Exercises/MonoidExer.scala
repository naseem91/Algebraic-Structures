package Exercises

import Structures.Monoid._

object MonoidExer {

  //Ex:10.1 Monoid Instances
  //1-integer addition
  val intAddition = new Monoid[Int] {
    def op(a:Int,b:Int)=a+b
    val zero=0
  }
  //2-nteger multiplication
  val intMultiplication = new Monoid[Int] {
    def op(a:Int,b:Int) = a*b
    val zero = 1
  }
  //3-boolean OR
  val bolleanOr=new Monoid[Boolean] {
    def op(a:Boolean,b:Boolean)= a || b
    val zero = true
  }

  //3-boolean AND
  val bolleanAnd=new Monoid[Boolean] {
    def op(a:Boolean,b:Boolean)= a && b
    val zero = true
  }
  //---------------------------------------------------------------------------
  //Ex:10.2 Monoid option instance
  def optionM[A]:Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a:Option[A],b:Option[A])= a orElse b
    val zero = None
  }
  //----------------------------------------------------------------------------
  // Ex:10.5 implement foldMap
  def foldMap[A,B](as:List[A],monoid: Monoid[B])(f:A=>B):B={
    as.foldLeft(monoid.zero)((a,b)=>monoid.op(a,f(b)))
  }

  def main(args: Array[String]): Unit = {
    val x = optionM[Int].op(None, Some(3))
    println(x)
    val x2 = optionM[String].op(Some("5"),Some("2"))
    println(x2)

    //foldmap
    println(foldMap(List(5,8,6,2),intAddition)(a=>a))
  }
}
