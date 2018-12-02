package Structures

object Monoid {

  trait Monoid[A]{
    def op(x:A,y:A):A
    def zero :A
  }

  //String Monoid
  val stringMonoid = new Monoid[String]{
    def op(a:String,b:String) = a+b
    val zero =""
  }

  //List Monoid
  def listMonoid[A] = new Monoid[List[A]] {
    def op(l1:List[A],l2:List[A])=l1 ++ l2
    val zero=Nil
  }
  // ------------------------------------------------------------------
  //Folding lists with monoid
  def concatenate[A](l:List[A],monoid: Monoid[A]):A={
    l.foldLeft(monoid.zero)(monoid.op(_,_))
  }

  def main(args: Array[String]): Unit = {
    println(concatenate(List("naseem","mahasneh"),stringMonoid))
  }
}
