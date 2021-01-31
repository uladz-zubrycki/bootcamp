import scala.annotation.tailrec

object Basics {
  def gcd(a: Int, b: Int): Either[String, Int] = {
    @tailrec
    def loop(a: Int, b: Int): Int =
      if (b == 0) a
      else loop(b, a % b)

    if (a <= 0) Left("'a' should be greater than 0")
    else if (b <= 0) Left("'b' should be greater than 0")
    else Right(loop(a, b))
  }

  def lcm(a: Int, b: Int): Either[String, Int] =
    gcd(a, b).map(d => a * b / d)
}
