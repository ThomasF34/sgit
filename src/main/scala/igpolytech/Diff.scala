package igpolytech
import scala.annotation.tailrec

case class Diff(changes: Array[Change], filePath: String) {
  override def toString(): String = {
    val (added, subed) = changes.partition(_.changeType == ChangeType.ADD)
    if (added.length > 0 && subed.length > 0) s"modified content -> $filePath"
    else if (added.length > 0)
      s"${Console.GREEN}added content -> ${filePath}${Console.RESET}"
    else s"${Console.RED}deleted content -> ${filePath}${Console.RESET}"
  }

  def getDetails(): String = {
    s"diff on $filePath:\n${changes.mkString("\n")}"
  }
}

object Diff {

  def getAddedFromTree(tree: Tree): Array[Diff] = {
    tree.trees.flatMap(getAddedFromTree) ++ tree.blobs.map(
      blob => Diff.addedFile(blob.content, s"${tree.name}${blob.name}")
    )
  }

  def getRemovedFromTree(tree: Tree): Array[Diff] = {
    tree.trees.flatMap(getRemovedFromTree) ++ tree.blobs.map(
      blob => Diff.removedFile(blob.content, s"${tree.name}${blob.name}")
    )
  }

  def fromTrees(oldTree: Tree, newTree: Tree): Array[Diff] = {
    val diffFromTrees = newTree.trees.flatMap(
      subtree =>
        oldTree.trees.find(_.name == subtree.name) match {
          case None             => getAddedFromTree(subtree)
          case Some(oldSubtree) => fromTrees(oldSubtree, subtree)
        }
    ) ++ oldTree.trees
      .filterNot(oldTree => newTree.trees.exists(_.name == oldTree.name))
      .flatMap(tree => Diff.getRemovedFromTree(tree))

    val diffFromBlobs = newTree.blobs.flatMap(
      blob =>
        oldTree.blobs.find(_.name == blob.name) match {
          case None =>
            Some(Diff.addedFile(blob.content, s"${newTree.name}${blob.name}"))
          case Some(oldBlob) =>
            Diff.fromContents(
              oldBlob.content,
              blob.content,
              s"${newTree.name}${blob.name}"
            )
        }
    ) ++ oldTree.blobs
      .filterNot(oldBlob => newTree.blobs.exists(_.name == oldBlob.name))
      .map(
        blob => Diff.removedFile(blob.content, s"${oldTree.name}${blob.name}")
      )

    diffFromTrees ++ diffFromBlobs
  }

  def fromContents(
      oldContent: String,
      newContent: String,
      filePath: String
  ): Option[Diff] = {

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
          changes :+ Change(ChangeType.ADD, l2(j - 1).toString(), j)
        )
      } else if (i > 0 && (j == 0 || matrix(i)(j - 1) < matrix(i - 1)(j))) {
        printDiff(
          matrix,
          l1,
          l2,
          i - 1,
          j,
          changes :+ Change(ChangeType.SUB, l1(i - 1).toString(), i)
        )
      } else {
        changes
      }
    }

    def splitByLine(text: String): List[String] = text.split("\n").toList

    val lines1 = splitByLine(oldContent)
    val lines2 = splitByLine(newContent)
    val result = printDiff(
      lcsLength(lines1, lines2, List.fill(1, lines2.size + 1)(0)),
      lines1,
      lines2,
      lines1.size,
      lines2.size,
      Array()
    ).reverse
    if (result.isEmpty) None
    else Some(Diff(result, filePath))
  }

  def addedFile(addedContent: String, filePath: String): Diff = {
    Diff(
      addedContent.split("\n").zipWithIndex.map {
        case (line, index) => Change(ChangeType.ADD, line, index + 1)
      },
      filePath
    )
  }

  def removedFile(removedContent: String, filePath: String): Diff = {
    Diff(
      removedContent.split("\n").zipWithIndex.map {
        case (line, index) => Change(ChangeType.SUB, line, index + 1)
      },
      filePath
    )
  }
}
