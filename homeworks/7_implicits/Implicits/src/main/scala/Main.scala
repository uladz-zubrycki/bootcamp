object TypeclassTask extends App {
  trait HashCode[A] {
    def hash(x: A): Int
  }

  object HashCode {
    def apply[A: HashCode](): HashCode[A] = implicitly[HashCode[A]]

    implicit class HashCodeSyntax[A: HashCode](x: A) {
      def hash(): Int = HashCode[A].hash(x)

      // Don't get why need HashCode.apply, if we still are going to implement hash as extension method,
      // so could as well retrieve implicit for HashCode inside it, thus making things a bit more explicit
      // and saving some keystrokes. Agreed that implicitly[HashCode[A]].hash(x) is a bit ugly,
      /// but it's just onetime implementation detail
      def hash2() = implicitly[HashCode[A]].hash(x)
    }
  }
  import HashCode._

  implicit val StringHashCode: HashCode[String] = s => s.length()

  List("", "fdasd", "fdsadfad").foreach(s =>
    println(s"Hash of ${s} equals ${s.hash()}")
  )
}

object Task1 extends App {
  final case class Money(amount: BigDecimal)
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(m => m.amount)

  // Are there any other way to do a conversion, which is more concise?
  val values = List(1, 4, 42, 2, 3).map(BigDecimal.apply).map(Money.apply)
  println(values.sorted)
}

object Task2 extends App {
  trait Show[T] {
    def show(entity: T): String
  }

  object Show {
    implicit class ShowSyntax[A: Show](x: A) {
      def show() = implicitly[Show[A]].show(x)
    }
  }
  import Show._

  final case class User(id: String, name: String)
  implicit val UserShow: Show[User] = u =>
    s"{ id: '${u.id}'; name: '${u.name}' }"

  println(User("1", "John Doe").show)
}

object Task3 extends App {
  type Error = String
  trait Parse[A] {
    def parse(entity: String): Either[Error, A]
  }

  object Parse {
    implicit class ParseSyntax(x: String) {
      def parse[A: Parse](): Either[Error, A] = implicitly[Parse[A]].parse(x)
    }
  }

  final case class User(id: String, name: String)
  object User {
    def show(u: User) = s"{ id: '${u.id}'; name: '${u.name}' }"
  }

  implicit val ParseUser: Parse[User] = s => {
    def parseProp(name: String, s: String) = {
      s.split(":").map(_.strip).toList match {
        case List(key, value) if key == name =>
          if (value.head == '\'' && value.last == '\'') Right {
            value.substring(1, value.length() - 1)
          }
          else
            Left {
              // Wasn't able to make escape sequence \" work and was too lazy to read this thread
              // https://github.com/scala/bug/issues/6476
              // any common approaches for that?
              s"Can't parse '${name}'. Expected to get string in format ${"\""}'value'${"\""}, but found ${"\""}${value}${"\""}"
            }
        case _ =>
          Left {
            s"Expected to get string in format ${"\""}${name} :'value'${"\""}, but found ${"\""}${s}${"\""}"
          }
      }
    }

    val props = s
      .strip()
      .stripPrefix("{")
      .stripSuffix("}")
      .split(";")
      .map(_.strip)
      .filter(_.nonEmpty)
      .toList

    props match {
      case List(id, name) =>
        for {
          id <- parseProp("id", id)
          name <- parseProp("name", name)
        } yield User(id, name)
      case _ => Left { s"Can't parse User from value ${"\""}${s}${"\""}" }
    }
  }

  import Parse._

  println("lalala".parse[User])
  println("{ id: 'id'; }".parse[User])
  println("{ name: 'John Doe'; id: 'id'; }".parse[User])
  println(User.show(User("1", "John Doe")).parse[User])
}

object Task4 extends App {
  trait Equality[A] {
    def customEquals(x: A, y: A): Boolean
  }

  object Equality {
    implicit class EqualitySyntax[A: Equality](x: A) {
      def customEquals(y: A): Boolean =
        implicitly[Equality[A]].customEquals(x, y)

      def ===(y: A): Boolean = x.customEquals(y)
    }
  }
  import Equality._

  implicit val EqualityInt: Equality[Int] = (x, y) => x == y

  println(5.customEquals(5))
  println(5 === 5)

  println(5.customEquals(6))
  println(5 === 6)

  // uncomment to get compilation error
  // println(5 === "5")
}

object AdvancedHomework {
  // TODO: create a typeclass for flatMap method
}
