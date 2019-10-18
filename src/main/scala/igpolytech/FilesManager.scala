package igpolytech
import java.io.File
import java.io.FileReader
import scala.annotation.tailrec
import java.io.BufferedReader
import scala.xml.Node
import java.io.FileWriter
import scala.io.Source
import scala.io.BufferedSource

object FilesIO {

  // UTILS
  val separator: String = File.separator

  // INPUTS

  /**
    * Returns true if the given dir is existing in the given File object
    * If no current File is given, it searches at the execution root.
    */
  def dirExists(dir: String, current: File = new File(".")): Boolean =
    new File(s"${current.getAbsolutePath()}${File.separatorChar}$dir")
      .isDirectory()

  def fileExists(path: String): Boolean =
    new File(path)
      .isFile()

  /**
    * Searchs recursively for the repo dir until reaching the root dir
    * Returns the path of the repo dir or $none if not found
    */
  @tailrec
  def getRepoDirPath(
      repoDir: String,
      current: File = new File(".")
  ): Option[String] = {
    val currentCanonical = current.getCanonicalFile()
    if (dirExists(repoDir, currentCanonical))
      Some(s"${currentCanonical.getAbsolutePath()}${File.separator}${repoDir}")
    else {
      if (currentCanonical.getParentFile() == null) None
      else getRepoDirPath(repoDir, currentCanonical.getParentFile())
    }
  }

  def loadXml(path: String): Node = {
    scala.xml.XML.loadFile(path)
  }

  def getContent(path: String): String = {
    val file = new File(path)
    if (file.exists() && file.isFile()) {
      Source.fromFile(file).mkString
    } else ""
  }

  def getAllFilesPath(pathDir: String): Array[String] = {
    val file = new File(pathDir)
    getAllFilesPath(file)
  }

  private def getAllFilesPath(dir: File, prefix: String = ""): Array[String] = {
    val (files, dirs) = dir
      .listFiles()
      .filter(_.getName() != ".sgit")
      .partition(_.isFile())

    val currentFiles = files.map(file => s"${prefix}${file.getName()}")
    val childFiles = dirs.flatMap(childDir => {
      val newPrefix = prefix match {
        case "" => s"${childDir.getName()}${File.separator}"
        case value: String =>
          s"${value}${childDir.getName()}${File.separator}"
      }
      getAllFilesPath(childDir, newPrefix)
    })

    currentFiles ++ childFiles
  }

  def getAllFiles(dir: File): Array[File] = {
    if (dir.isFile()) Array(dir)
    else
      dir
        .listFiles()
        .filter(_.getName() != ".sgit")
        .flatMap(getAllFiles(_))
  }
  // OUTPUTS

  def createDirectories(dirs: Array[String]) = {
    dirs.foreach(dir => new File(dir).mkdirs());
  }

  def createFiles(paths: Array[String]) = {
    paths.foreach(path => new File(path).createNewFile())
  }

  def delete(path: String): Unit = {
    val file = new File(path)
    if (file.isDirectory()) {
      file.listFiles().foreach(f => delete(f.getCanonicalPath()))
    }
    file.delete()
  }

  def deleteFile(path: String) = {
    val file = new File(path)
    if (file.isFile()) file.delete()
  }

  def deleteFiles(paths: Array[String]) = paths.map(deleteFile(_))

  /**
    * Write the content in the given file.
    * File will be created if does not exist
    */
  def write(path: String, content: String) = {
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }

    val writer = new FileWriter(file)
    writer.write(content)
    writer.close()
  }

  def saveXml(content: Node, path: String) = {
    scala.xml.XML.save(path, content);
  }

  // END

  //TODO SEE IF USABLE
  def emptyFile(filePath: String): Boolean = {
    val file = new File(filePath)
    val reader: BufferedReader = new BufferedReader(new FileReader(file))
    reader.readLine() == null
  }

  def getHash(filePath: String): Option[String] = {
    val file = new File(filePath)
    val reader: BufferedReader = new BufferedReader(new FileReader(file))
    val line = reader.readLine()
    if (line == null) None
    else Some(line)
  }
}
