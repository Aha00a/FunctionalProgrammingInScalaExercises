
import scala.annotation.tailrec

object Main extends App {
  println(Chapter2.Exercise1())
  println(Chapter2.Exercise2())
}

object Chapter2 {

  object Exercise1 {
    final def apply() = {
      (1 to 10).map(fib)
    }

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

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      as.sliding(2).forall(a => ordered(a(0), a(1)))
    }
  }

}