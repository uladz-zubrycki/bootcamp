import scala.annotation.tailrec

object Main {
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    list
      .foldLeft(List(zero))((acc, cur) =>
        acc match {
          case x :: xs => f(x, cur) :: acc
          case _       => acc
        }
      )
      .reverse
  }

  def scanLeftRec[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    @tailrec
    def loop(last: T, item: T, list: List[T], res: List[T]): List[T] = {
      var cur = f(item, last)

      list match {
        case Nil => cur :: res
        case x :: xs =>
          loop(cur, x, xs, cur :: res)
      }
    }

    list match {
      case Nil     => List(zero)
      case x :: xs => loop(zero, x, xs, List(zero)).reverse
    }
  }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = {
    if (s.isEmpty()) List.empty
    else
      s.foldLeft((List(): List[(Char, Int)]))((acc, cur) => {
        acc match {
          case Nil => List((cur, 1))
          case (ch, count) :: xs =>
            if (cur == ch) (ch, count + 1) :: xs
            else (cur, 1) :: acc
        }
      }).reverse
  }

  //https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).tail
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    nums.grouped(n).toList match {
      case List(xs, ys) =>
        xs.zip(ys)
          .map({ case (x, y) => Array(x, y) })
          .flatten
      case _ => Array.empty
    }
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int =
    accounts.map(_.reduce(_ + _)).max

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  // Creating all possible distributions of extra candies,
  // applying those and checking whether any distribution gives max to a kid
  // Fails on memory limit in leetcode :)
  def kidsWithCandies(
      candies: Array[Int],
      extraCandies: Int
  ): Array[Boolean] = {
    def distributions(
        extra: Int,
        kidsLeft: Int,
        cur: List[(Int, List[Int])]
    ): List[List[Int]] = {
      if (kidsLeft == 0)
        cur.map(_._2).filter(a => a.sum == extra)
      else {
        val nextAllocations = for {
          (candiesLeft, allocation) <- cur
          candiesTaken <- List.range(0, candiesLeft + 1)
        } yield (candiesLeft - candiesTaken, candiesTaken :: allocation)

        distributions(extra, kidsLeft - 1, nextAllocations)
      }
    }

    distributions(
      extraCandies,
      candies.length,
      List((extraCandies, List.empty))
    )
      .map(d => {
        val candiesDistribution =
          d.zip(candies)
            .map({ case (kid, extra) => kid + extra })

        val max = candiesDistribution.max
        candiesDistribution.map(_ == max)
      })
      .reduce((acc, cur) => acc.zip(cur).map({ case (fst, snd) => fst || snd }))
      .toArray
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  // Simply checking that kid has max candies count if gets all the extras
  def kidsWithCandiesSimple(
      candies: Array[Int],
      extraCandies: Int
  ): Array[Boolean] = {
    val max = candies.max(scala.math.Ordering.Int)
    candies.map(_ + extraCandies >= max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val xs =
      points
        .map(arr => arr(0))
        .distinct
        .sorted;

    xs.zip(xs.tail)
      .map({ case (fst, snd) => snd - fst })
      .max
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    def loop(s: String, curDepth: Int, maxDepth: Int): Int =
      s match {
        case x if !(x.contains("(") || x.contains(")")) =>
          maxDepth
        case x if x.startsWith(")") =>
          val blockDepth = x.takeWhile(c => c == ')').length()
          loop(
            x.substring(blockDepth),
            curDepth - blockDepth,
            Math.max(curDepth, maxDepth)
          )
        case x if x.startsWith("(") =>
          loop(x.substring(1), curDepth + 1, maxDepth)
        case x =>
          loop(
            s.substring(
              List(s.indexOf('('), s.indexOf(')')).filter(_ != -1).min
            ),
            curDepth,
            maxDepth
          )
      }

    loop(s, 0, 0)
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  // https://leetcode.com/problems/matrix-block-sum/
}
