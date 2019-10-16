package igpolytech
import scala.annotation.tailrec

object Merge {
  def fromCommits(
      firstCommit: Commit,
      secondCommit: Commit,
      commitsPath: String
  ) = {
    val ancestorCommit =
      Commit.getAncestorCommit(firstCommit, secondCommit, commitsPath)

    val new1 = "Y\nA\nA\nB"
    val common = "Y\nY\nY\nY\nY\nA\nA\nK\nB"
    val new2 = "Y\nA\nK\nB"
    val diff =
      Merge.fromTripleContent(
        common,
        new1,
        new2
      )

    val result = List(common, new1, new2)
      .map(elem => Merge.align(diff.toList, elem.split("\n").toList, List()))

    def flat =
      (t: ((Option[String], Option[String]), Option[String])) =>
        (t._1._1, t._1._2, t._2)

    result.foreach(println)
    println(result(0).zip(result(1)).zip(result(2)).map(flat))
    val toBeMergedListOption =
      result(0).zip(result(1)).zip(result(2)).map(flat).map {
        case (ancestor, change1, change2) =>
          if (ancestor != change1 && ancestor != change2 && change1 != change2)
            None
          else if (ancestor == change1 && ancestor == change2) Some(ancestor)
          else if (ancestor != change1) Some(change1)
          else Some(change2)
      }

    if (toBeMergedListOption.contains(None)) System.exit(1)
    else toBeMergedListOption.flatten.foreach(println)

    ancestorCommit.toString()
  }

  def fromTripleContent(
      commonContent: String,
      firstNewContent: String,
      secondNewContent: String
  ) = {
    def newIndex(x: Int) = math.max(1, x - 1)

    @tailrec
    def getDiff(
        l1: List[String],
        l2: List[String],
        l3: List[String],
        i: Int,
        j: Int,
        k: Int,
        diff: Array[Option[String]]
    ): Array[Option[String]] = {
      if (i > 0 && j > 0 && k > 0 && l1(i - 1) == l2(j - 1) && l2(j - 1) == l3(
            k - 1
          )) {
        getDiff(
          l1,
          l2,
          l3,
          i - 1,
          j - 1,
          k - 1,
          diff :+ Some(l1(i - 1))
        )
      } else if (i == 1 && j == 1 && k == 1)
        diff :+ None
      else if (i > 0 && j > 0 && k > 0) {
        getDiff(
          l1,
          l2,
          l3,
          newIndex(i),
          newIndex(j),
          newIndex(k),
          diff :+ None
        )
      } else {
        val maxLength = List(l1.size, l2.size, l3.size).reduce(_ max _)
        diff ++ Array.fill(maxLength - diff.size)(None)
      }
    }

    def splitByLine(text: String): List[String] = text.split("\n").toList

    val lines1 = splitByLine(commonContent)
    val lines2 = splitByLine(firstNewContent)
    val lines3 = splitByLine(secondNewContent)
    val result = getDiff(
      lines1,
      lines2,
      lines3,
      lines1.size,
      lines2.size,
      lines3.size,
      Array()
    ).reverse
    if (result.isEmpty) Array()
    else result

  }

  /**
    * Will align both lists based on LCS
    * Pre-condition : ly.size <= lx.size
    * Some(x)' of lx are all present in ly and in the same order
    */
  def align(
      lx: List[Option[String]],
      ly: List[String],
      acc: List[Option[String]]
  ): List[Option[String]] = {
    (lx, ly) match {
      case ((x :: xs), (y :: ys)) if x == Some(y) =>
        align(xs, ys, acc :+ Some(y))
      case ((None :: xs), (y :: ys))
          if ((None :: xs).size) < ((y :: ys).size + 1) =>
        align(xs, ys, acc :+ Some(y))
      case ((None :: xs), (y :: ys))
          if xs
            .collectFirst { case Some(x) => Some(x) }
            .contains(Some(y)) =>
        align(xs, y :: ys, acc :+ None)
      case ((None :: xs), (y :: ys)) => align(xs, ys, acc :+ Some(y))
      case (Nil, Nil)                => acc
    }
  }
}
