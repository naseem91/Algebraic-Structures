package Structures

object IdentityMonad {
  import Monad._

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    override def flatMap[A,B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }

  def main(args: Array[String]): Unit = {
    println(Id("Hello, ") flatMap (a => Id("monad!") flatMap (b => Id(a + b))))
  }
}
