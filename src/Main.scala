
import scala.annotation.tailrec

object Main extends App {
  Seq(Chapter2.Exercise1(), Chapter2.Exercise2(), Chapter2.Exercise3(), Chapter2.Exercise4()).foreach(println)
}

object Chapter2 {

  object Exercise1 {
    final def apply() = (1 to 10).map(fib)

    def fib(n: Int): Int = {
      @tailrec
      def rec(n: Int, v1: Int, v2: Int): Int = {
        if (n == 0)
          v1
        else
          rec(n - 1, v2, v2 + v1)
      }

      rec(n, 0, 1)
    }

  }

  object Exercise2 {
    final def apply() = {
      val ordered: (Int, Int) => Boolean = (lhs: Int, rhs: Int) => lhs < rhs

      (isSorted((1 to 10).toArray, ordered), isSorted((10 to 1).toArray, ordered))
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as.sliding(2).forall(a => ordered(a(0), a(1)))
  }

  object Exercise3 {
    final def apply() = {
      def sumAndString(a: Int, b: Long): String = (a + b).toString
      curry(sumAndString)(5)(10)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
  }

  object Exercise4 {
    final def apply() = {
      def sumAndString(a: Int, b: Long): String = (a + b).toString
      val curried = Exercise3.curry(sumAndString)
      uncurry(curried)(5, 10)
    }

    //noinspection SpellCheckingInspection
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  }

}