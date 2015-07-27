import scala.annotation.tailrec

object Main extends App {
  println(Chapter2.Exercise1())
}

object Chapter2 {

  object Exercise1 {
    final def apply() = {
      (1 to 10).map(fib)
    }

    def fib(n: Int): Int = {
      @tailrec
      def aux(n: Int, v1: Int, v2: Int): Int = {
        if (n == 0)
          v1
        else
          aux(n - 1, v2, v2 + v1)
      }

      aux(n, 0, 1)
    }

  }

}