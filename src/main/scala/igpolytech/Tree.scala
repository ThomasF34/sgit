package igpolytech
import java.io.File

case class Tree(
    hash: String,
    name: String,
    trees: Array[Tree] = Array(),
    blobs: Array[Blob] = Array()
) {

  /**
    * Returns an array of the tree content
    */
  def getAllFiles(): Array[String] =
    trees.flatMap(
      tree => tree.getAllFiles().map(file => s"${name}${File.separator}$file")
    ) ++ blobs.map(blob => s"${name}${File.separator}${blob.name}")

  /**
    * Saves the tree in the tree folder
    */
  def save() = {}
}

object Tree {
  def getTree(hash: String): Tree = {
    new Tree(
      hash,
      "src",
      Array(new Tree("que", "abc", Array(), Array(new Blob("blob2")))),
      Array(new Blob("blob1"))
    );
  }
}
