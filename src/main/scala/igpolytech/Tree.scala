package igpolytech
import java.io.File
import scala.xml.Node

case class Tree(
    name: String,
    trees: Array[Tree] = Array(),
    blobs: Array[Blob] = Array()
) {

  def hash: String =
    GeneralHelper.generateHash(this.toString())

  /**
    * Returns an array of the tree content
    **/
  def getAllFiles(): Array[String] =
    trees.flatMap(
      tree => tree.getAllFiles()
    ) ++ blobs.map(blob => s"${name}${blob.name}")

  /**
    * Saves the tree in the tree folder
    */
  def save(
      saveTreeAsXml: (Node, String) => Unit,
      writeBlobToRepo: (String, String) => Unit
  ): Unit = {
    trees.foreach(_.save(saveTreeAsXml, writeBlobToRepo))
    blobs.foreach(_.save(writeBlobToRepo))
    saveTreeAsXml(this.toXml(), hash)
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

  def getModified(
      blobExists: (String) => (String) => Boolean,
      blobContent: (String) => String,
      fileContent: (String) => (String) => String
  ): Array[Diff] = {
    blobs
      .flatMap(
        _.getDiffWithNew(
          name,
          blobExists(name),
          blobContent,
          fileContent(name)
        )
      ) ++ trees.flatMap(
      _.getModified(blobExists, blobContent, fileContent)
    )
  }

  override def toString(): String =
    s"${name}${trees.map(_.hash).mkString}${blobs.mkString}"

  def equals(tree: Tree): Boolean = this.hash.equals(tree.hash)

  def createAllFiles(
      writeBlobToRepo: (String) => (String, String) => Unit
  ): Unit = {
    trees.map(_.createAllFiles(writeBlobToRepo))
    blobs.map(_.writeBlob(writeBlobToRepo(name)))
  }
}

object Tree {
  def getTree(
      hash: String,
      blobContent: (String) => (String),
      getTreeContent: (String) => (Node)
  ): Tree = {
    val xml = getTreeContent(hash)
    getTree(xml, blobContent, getTreeContent)
  }

  def getTree(
      xmlContent: Node,
      blobContent: (String) => String,
      treeContent: (String) => Node
  ): Tree = {
    val name = (xmlContent \ "name").text
    val trees =
      (xmlContent \ "trees" \ "tree")
        .map(hash => {
          Tree.getTree(
            hash.text.trim(),
            blobContent,
            treeContent
          )
        })
        .toArray
    val blobs =
      (xmlContent \ "blobs" \ "blob")
        .map(
          hash => Blob.getBlob(hash \@ "name", hash.text.trim(), blobContent)
        )
        .toArray
    new Tree(name, trees, blobs)

  }

  def createFromList(
      files: Array[String],
      projectDir: String,
      allFiles: (File) => Array[File],
      fileContent: (String) => (String) => String
  ): Tree = {
    val explicitedFiles: Array[Array[String]] = files
      .flatMap(f => allFiles(new File(f)))
      .map(file => {
        file
          .getCanonicalPath()
          .replace(s"$projectDir", "")
          .split(File.separator)
      })

    createTree(explicitedFiles, "", projectDir, fileContent);
  }

  /**
    * Creates a tree from an array of string-array.
    * The later should describe the path of a file. Each path should begin from the repodir
    * and each element represent a dir. The last element is the name of the file to add.
    */
  private def createTree(
      files: Array[Array[String]],
      currentName: String,
      projectDir: String,
      fileContent: (String) => (String) => String
  ): Tree = {
    val (blobs, trees) = files.partition(pathAsArray => pathAsArray.length == 1)

    val newBlobsArray = blobs
      .map(blob => {
        Blob.getBlob(
          blob(0),
          blob(0),
          fileContent(currentName)
        )
      })
      .toArray
    val newTreesArray = trees
      .groupBy[String](splitedPath => splitedPath(0))
      .map {
        case (name, tree) => {
          createTree(
            tree.map(_.tail),
            s"${currentName}${name}${File.separator}",
            projectDir,
            fileContent
          )
        }
      }
      .toArray

    new Tree(currentName, newTreesArray, newBlobsArray)
  }
}
