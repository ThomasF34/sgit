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
    ) ++ blobs.map(blob => s"${name}${blob.name}")

  /**
    * Saves the tree in the tree folder
    */
  def save() = {}
}
