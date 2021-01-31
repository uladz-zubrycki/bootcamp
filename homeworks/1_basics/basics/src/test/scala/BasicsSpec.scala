import org.scalacheck._
import Basics._

object Basicspecification extends Properties("gcd and lcm") {
  import Prop.forAll

  val positiveInteger = Gen.choose(1, 1000)
  val negativeInteger = Gen.choose(-1000, -1)

  def checkRight(x: Either[String, Int], f: Int => Boolean) =
    x match {
      case Right(x) => f(x)
      case Left(_)  => false
    }

  def checkLeft(x: Either[String, Int]) =
    x match {
      case Right(_) => false
      case Left(_)  => true
    }

  property("divisor isn't defined for negative number") =
    forAll(positiveInteger, negativeInteger) { (p: Int, n: Int) =>
      checkLeft(gcd(n, n)) &&
      checkLeft(gcd(p, n)) &&
      checkLeft(gcd(n, p))
    }

  property("divisor isn't defined for zero") = forAll(positiveInteger) {
    (p: Int) =>
      checkLeft(gcd(p, 0)) &&
      checkLeft(gcd(0, p))
  }

  property("divisor is less than or equal to numbers") =
    forAll(positiveInteger, positiveInteger) { (a: Int, b: Int) =>
      checkRight(gcd(a, b), d => d <= a && d <= b)
    }

  property("both are are divided by divisor") =
    forAll(positiveInteger, positiveInteger) { (a: Int, b: Int) =>
      checkRight(gcd(a, b), d => a % d == 0 && b % d == 0)
    }

  property(
    "divisor is the less number, when greater number is divided by the less"
  ) = forAll(positiveInteger, positiveInteger) { (x: Int, k: Int) =>
    val less = x
    val greater = x * k
    checkRight(gcd(less, greater), d => d == less) &&
    checkRight(gcd(greater, less), d => d == less)
  }

  property("multiple is divided by both") =
    forAll(positiveInteger, positiveInteger) { (a: Int, b: Int) =>
      checkRight(lcm(a, b), m => m % a == 0 && m % b == 0)
    }

  property("multiple divided by number is less than or equal to the other") =
    forAll(positiveInteger, positiveInteger) { (a: Int, b: Int) =>
      def check(multiple: Int, a: Int, b: Int) =
        multiple / a <= b

      checkRight(lcm(a, b), m => check(m, a, b) && check(m, b, a))
    }
}
