// I'm not sure about such design, but feel like it could give some benefits.
// Also it was a first step into learning ad-hoc polymorphism in Scala.

sealed trait Axis {
  def value: Double
}
final case class XAxis(value: Double) extends Axis
final case class YAxis(value: Double) extends Axis
final case class ZAxis(value: Double) extends Axis

sealed trait Shape

sealed trait Located[A <: Shape, B <: Axis] {
  def coord(x: A): B
}

sealed trait Bounded[A <: Shape, B <: Axis] {
  def minCoord(x: A): B
  def maxCoord(x: A): B
}

sealed trait Shape2D extends Shape {
  def area: Double
}

object Shape2D {
  def boundArea[A <: Shape](
      x: A
  )(implicit xBound: Bounded[A, XAxis], yBound: Bounded[A, YAxis]) = {
    (xBound.maxCoord(x).value - xBound.minCoord(x).value) *
      (yBound.minCoord(x).value - yBound.maxCoord(x).value)
  }
}

final case class Point(x: XAxis, y: YAxis) extends Shape2D {
  override def area = 0
}
object Point {
  implicit val xBound = new Bounded[Point, XAxis] {
    def minCoord(p: Point) = p.x
    def maxCoord(p: Point) = p.x
  }

  implicit val yBound = new Bounded[Point, YAxis] {
    def minCoord(p: Point) = p.y
    def maxCoord(p: Point) = p.y
  }
}

final case class Circle(centerX: XAxis, centerY: YAxis, radius: Double)
    extends Shape2D {
  override def area = Math.PI * Math.pow(radius, 2)
}
object Circle {
  implicit val xBound = new Bounded[Circle, XAxis] {
    def minCoord(c: Circle) = XAxis(c.centerX.value - c.radius)
    def maxCoord(c: Circle) = XAxis(c.centerX.value + c.radius)
  }

  implicit val yBound = new Bounded[Circle, YAxis] {
    def minCoord(c: Circle) = YAxis(c.centerY.value - c.radius)
    def maxCoord(c: Circle) = YAxis(c.centerY.value + c.radius)
  }
}

final case class Square(centerX: XAxis, centerY: YAxis, size: Double)
    extends Shape2D {
  override def area = Math.pow(size, 2)
}
object Square {
  implicit val xBound = new Bounded[Square, XAxis] {
    def minCoord(s: Square) = XAxis(s.centerX.value - (s.size / 2))
    def maxCoord(s: Square) = XAxis(s.centerX.value + (s.size / 2))
  }

  implicit val yBound = new Bounded[Square, YAxis] {
    def minCoord(s: Square) = YAxis(s.centerY.value - (s.size / 2))
    def maxCoord(s: Square) = YAxis(s.centerY.value + (s.size / 2))
  }
}

sealed trait Shape3D extends Shape {
  def surfaceArea: Double
  def volume: Double
}

final case class Point3D(x: XAxis, y: YAxis, z: ZAxis) extends Shape3D {
  override def surfaceArea = 0
  override def volume = 0
}
object Point3D {
  implicit val xBound = new Bounded[Point3D, XAxis] {
    def minCoord(p: Point3D) = p.x
    def maxCoord(p: Point3D) = p.x
  }

  implicit val yBound = new Bounded[Point3D, YAxis] {
    def minCoord(p: Point3D) = p.y
    def maxCoord(p: Point3D) = p.y
  }

  implicit val zBound = new Bounded[Point3D, ZAxis] {
    def minCoord(p: Point3D) = p.z
    def maxCoord(p: Point3D) = p.z
  }
}

object Shape3D {
  def boundVolume[A <: Shape](
      x: A
  )(implicit
      xBound: Bounded[A, XAxis],
      yBound: Bounded[A, YAxis],
      zBound: Bounded[A, ZAxis]
  ) = {
    (xBound.maxCoord(x).value - xBound.minCoord(x).value) *
      (yBound.maxCoord(x).value - yBound.minCoord(x).value) *
      (zBound.maxCoord(x).value - zBound.minCoord(x).value)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    // Is it possible to implement Axis => Double implicit conversion, so that I could write
    // Point3D(1, 1, 1) instead of Point3D(XAxis(1), YAxis(1), ZAxis(1)) and  point3.x instead of point3.x.value?
    val point3 = Point3D(XAxis(1), YAxis(1), ZAxis(1))

    val boundVolume = Shape3D.boundVolume(point3)

    // we could project figures to different axis and call functions to calculate sth
    // e.g point3 also exists in X, Y space
    val boundArea = Shape2D.boundArea(point3)
  }
}
