package example
import java.io.File

object FilesIO {
  def createDirectories(dirs : Array[String]) = {
    dirs.map(dir => new File(dir).mkdirs());
  }

  def createFiles(paths: Array[String]) = {
    paths.map(path => new File(path).createNewFile())
  }

  def dirExists(dir: String) : Boolean = new File(dir).isDirectory()
}