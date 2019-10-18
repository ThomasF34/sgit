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
  def save(treesPath: String, blobsPath: String): Unit = {
    trees.foreach(_.save(treesPath, blobsPath))
    //TODO delete me
    val writeBlobToRepo = (content: String, hash: String) =>
      FilesIO.write(s"${blobsPath}${hash}", content)
    blobs.foreach(_.save(writeBlobToRepo))
    FilesIO.saveXml(this.toXml(), s"${treesPath}${hash}")
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

  def getModified(projectDir: String, blobsPath: String): Array[Diff] = {
    //TODO delete me
    val blobExists = (blobName: String) =>
      FilesIO.fileExists(s"${projectDir}${name}${blobName}")
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${blobsPath}${blobHash}")
    val fileContent = (blobName: String) =>
      FilesIO.getContent(s"${projectDir}${this.name}${blobName}")
    blobs
      .flatMap(
        _.getDiffWithNew(
          projectDir,
          name,
          blobsPath,
          blobExists,
          blobContent,
          fileContent
        )
      ) ++ trees.flatMap(_.getModified(projectDir, blobsPath))
  }

  override def toString(): String =
    s"${name}${trees.map(_.name).mkString}${blobs.mkString}"

  def equals(tree: Tree): Boolean = this.hash.equals(tree.hash)

  def createAllFiles(projectDir: String): Unit = {
    trees.map(_.createAllFiles(projectDir))
    //TODO
    val writeBlobToRepo = (nameBlob: String, content: String) =>
      FilesIO.write(s"${projectDir}${this.name}$nameBlob", content)
    blobs.map(_.writeBlob(writeBlobToRepo))
  }
}

object Tree {
  def getTree(pathTreeDir: String, pathBlobDir: String, hash: String): Tree = {
    val xml = FilesIO.loadXml(s"${pathTreeDir}${hash}")
    getTree(pathTreeDir, pathBlobDir, xml)
  }

  def getTree(
      pathTreeDir: String,
      pathBlobDir: String,
      xmlContent: Node
  ): Tree = {
    val name = (xmlContent \ "name").text
    val trees =
      (xmlContent \ "trees" \ "tree")
        .map(hash => {
          Tree.getTree(pathTreeDir, pathBlobDir, hash.text.trim())
        })
        .toArray
    // TODO delete me
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${pathBlobDir}$blobHash")
    val blobs =
      (xmlContent \ "blobs" \ "blob")
        .map(
          hash => Blob.getBlob(hash \@ "name", hash.text.trim(), blobContent)
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

    createTree(explicitedFiles, "", projectDir);
  }

  /**
    * Creates a tree from an array of string-array.
    * The later should describe the path of a file. Each path should begin from the repodir
    * and each element represent a dir. The last element is the name of the file to add.
    */
  private def createTree(
      files: Array[Array[String]],
      currentName: String,
      projectDir: String
  ): Tree = {
    val (blobs, trees) = files.partition(pathAsArray => pathAsArray.length == 1)

    //TODO delete me
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${projectDir}${currentName}$blobHash")
    val newBlobsArray = blobs
      .map(blob => {
        Blob.getBlob(
          blob(0),
          blob(0),
          blobContent
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
            projectDir
          )
        }
      }
      .toArray

    new Tree(currentName, newTreesArray, newBlobsArray)
  }
}
