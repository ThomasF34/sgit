package igpolytech
import java.io.File
import java.io.FileWriter
import scala.xml.Node

case class Tree(
    name: String,
    trees: Array[Tree] = Array(),
    blobs: Array[Blob] = Array()
) {

  def hash: String = "Not yet implemented"

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
  def save(path: String) = {
    FilesIO.saveXml(this.toXml(), path)
  }

  def toXml(): Node = {
    <Tree>
      <name>{name}</name>
      <trees>
        {trees.map(tree => <tree name={tree.name}>{tree.hash}</tree>)}
      </trees>
      <blobs>
        {blobs.map(blob => <blob name={blob.name}>{blob.hash}</blob>)}
      </blobs>
    </Tree>
  }

  def mergeTree(withTree: Tree): Tree = {
    val withTreeBlobsName = withTree.blobs.map(_.name)
    val withTreeTreesName = withTree.trees.map(_.name)
    val thisTreesName = this.trees.map(_.name)

    val newBlobs =
      this.blobs
        .filterNot(blob => withTreeBlobsName.contains(blob.name)) ++ withTree.blobs

    val mergedCommonTrees = (this.trees.filter(
      tree => withTreeTreesName.contains(tree.name)
    ) ++ withTree.trees.filter(tree => thisTreesName.contains(tree.name)))
      .groupBy(_.name)
      .map(el => el._2(0).mergeTree(el._2(1)))
      .toArray

    val newTrees =
      this.trees
        .filterNot(tree => withTreeTreesName.contains(tree.name)) ++ withTree.trees
        .filterNot(tree => thisTreesName.contains(tree.name)) ++ mergedCommonTrees

    new Tree(name, newTrees, newBlobs)
  }

  override def toString(): String =
    s"${trees.mkString(" - ")}\nTree $name hashed $hash with blob : ${blobs.mkString(" - ")}\n"
}

object Tree {
  def getTree(pathTreeDir: String, hash: String): Tree = {
    val xml = FilesIO.loadXml(s"${pathTreeDir}${File.separator}${hash}")
    getTree(pathTreeDir, xml)

    new Tree(
      "src",
      Array(new Tree("abc", Array(), Array(new Blob("blob2")))),
      Array(new Blob("blob1"))
    );
  }

  def getTree(pathTreeDir: String, xmlContent: Node): Tree = {
    val name = (xmlContent \ "name").text
    val trees =
      (xmlContent \ "trees")
        .map(hash => Tree.getTree(pathTreeDir, hash.text))
        .toArray
    val blobs =
      (xmlContent \ "blobs")
        .map(
          hash => Blob(hash \@ "name")
        )
        .toArray
    new Tree(name, trees, blobs)

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
  def _createTree(
      files: Array[Array[String]],
      currentName: String = "/"
  ): Tree = {
    val (blobs, trees) = files.partition(pathAsArray => pathAsArray.length == 1)

    val newBlobsArray = blobs.map(blob => new Blob(blob(0))).toArray
    val newTreesArray = trees
      .groupBy[String](splitedPath => splitedPath(0))
      .map {
        case (name, tree) => {
          _createTree(tree.map(_.tail), name)
        }
      }
      .toArray

    new Tree(currentName, newTreesArray, newBlobsArray)
  }
}
