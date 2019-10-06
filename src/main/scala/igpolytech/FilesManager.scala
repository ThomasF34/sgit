package igpolytech
import java.io.File
import java.io.FileReader
import scala.annotation.tailrec
import java.io.BufferedReader

object FilesIO {
  def createDirectories(dirs: Array[String]) = {
    dirs.foreach(dir => new File(dir).mkdirs());
  }

  def createFiles(paths: Array[String]) = {
    paths.foreach(path => new File(path).createNewFile())
  }

  /**
    * Returns true if the given dir is existing in the given File object
    * If no current File is given, it searches at the execution root.
    */
  def dirExists(dir: String, current: File = new File(".")): Boolean =
    new File(s"${current.getAbsolutePath()}${File.separatorChar}$dir")
      .isDirectory()

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

  def getHash(filePath: String): Option[String] = {
    val file = new File(filePath)
    val reader: BufferedReader = new BufferedReader(new FileReader(file))
    val line = reader.readLine()
    if (line == null) None
    else Some(line)
  }

  def getAllFiles(pathDir: String): Array[String] = {
    val file = new File(pathDir)
    getAllFiles(file)
  }

  def getAllFiles(dir: File, prefix: String = ""): Array[String] = {
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
      getAllFiles(childDir, newPrefix)
    })

    currentFiles ++ childFiles
  }

  def delete(path: String): Unit = {
    val file = new File(path)
    if (file.isDirectory()) {
      file.listFiles().foreach(f => delete(f.getCanonicalPath()))
    }
    file.delete()
  }
}
