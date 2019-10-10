package igpolytech
import scala.annotation.tailrec

case class Diff(changes: Array[Change]) {
  override def toString(): String = {
    changes.mkString("\n")
  }
}

object Diff {
  def fromFiles(firstFile: String, secondFile: String): Option[Diff] = {
    // Some(new Diff(ADD, "abc"))
    ???
  }

  def fromTrees(firstTree: Tree, secondTree: Tree): Array[Diff] = {
    // Array(new Diff(SUB, "abc"))
    ???
  }

  def diffBetweenTexts(text1: String, text2: String) = {

    @tailrec
    def lcsLength[T](
        l1: List[T],
        l2: List[T],
        res: List[List[Int]]
    ): List[List[Int]] = {
      val index1 = res.size - 1
      if (l1.size <= index1) {
        res
      } else {
        val item1 = l1(index1)
        val newLine = l2.zipWithIndex.scanLeft(0) {
          case (previous, (item2, index2)) =>
            if (item1 == item2) {
              res(index1)(index2) + 1
            } else {
              Math.max(previous, res(index1)(index2 + 1))
            }
        }
        lcsLength(l1, l2, res ++ List(newLine))
      }
    }

    @tailrec
    def printDiff[T](
        matrix: List[List[Int]],
        l1: List[T],
        l2: List[T],
        i: Int,
        j: Int,
        changes: Array[Change]
    ): Array[Change] = {
      if (i > 0 && j > 0 && l1(i - 1) == l2(j - 1)) {
        printDiff(matrix, l1, l2, i - 1, j - 1, changes)
      } else if (j > 0 && (i == 0 || matrix(i)(j - 1) >= matrix(i - 1)(j))) {
        printDiff(
          matrix,
          l1,
          l2,
          i,
          j - 1,
          changes :+ Change(ChangeType.ADD, l2(j - 1).toString())
        )
      } else if (i > 0 && (j == 0 || matrix(i)(j - 1) < matrix(i - 1)(j))) {
        printDiff(
          matrix,
          l1,
          l2,
          i - 1,
          j,
          changes :+ Change(ChangeType.SUB, l1(i - 1).toString())
        )
      } else {
        changes
      }
    }

    def splitByLine(text: String): List[String] = text.split("\n").toList

    val lines1 = splitByLine(text1)
    val lines2 = splitByLine(text2)
    printDiff(
      lcsLength(lines1, lines2, List.fill(1, lines2.size + 1)(0)),
      lines1,
      lines2,
      lines1.size,
      lines2.size,
      Array()
    ).reverse

  }
}
