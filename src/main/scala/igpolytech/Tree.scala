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
    **/
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

  def createFromList(files: Array[String], projectDir: String): Tree = {
    val explicitedFiles: Array[Array[String]] = files
      .flatMap(f => FilesIO.getAllFiles(new File(f)))
      .map(file => {
        file
          .getCanonicalPath()
          .replace(s"$projectDir", "")
          .split(File.separator)
      })

    _createTree(explicitedFiles);
  }

  /**
    * Creates a tree from an array of string-array.
    * The later should describe the path of a file. Each path should begin from the repodir
    * and each element represent a dir. The last element is the name of the file to add.
    */
  def _createTree(files: Array[Array[String]]): Tree = {
    println(s"gonna create from ${files.map(_.mkString("/")).mkString("\n")}")

    // val (blobs, trees) =
    //   files.partition(pathAsArray => pathAsArray.length == 1)

    new Tree("a", "a");
  }
}
