package example
import java.io.File
import scala.annotation.tailrec

object FilesIO {
  def createDirectories(dirs: Array[String]) = {
    dirs.map(dir => new File(dir).mkdirs());
  }

  def createFiles(paths: Array[String]) = {
    paths.map(path => new File(path).createNewFile())
  }

  /**
    * Returns true if the given dir is existing in the given File object
    * If no current File is given, it searches at the execution root.
    */
  def dirExists(dir: String, current: File = new File(".")): Boolean =
    new File(s"${current.getAbsolutePath()}${File.separatorChar}$dir")
      .isDirectory()

  /**
    * Returns true if the given in the parents dir. Searches until reaching root dir
    */
  @tailrec
  def parentDirExists(dir: String, current: File = new File(".")): Boolean = {
    val currentCanonical = current.getCanonicalFile()
    if (currentCanonical.getParentFile() == null)
      return dirExists(dir, currentCanonical)
    else {
      if (dirExists(dir, currentCanonical)) return true
      else return parentDirExists(dir, currentCanonical.getParentFile())
    }
  }
}
