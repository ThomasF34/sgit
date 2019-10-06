package igpolytech
import java.io.File

class Repo(repoDir: String) {
  def getStatus(): String = {
    s"# Stagged \n${_getStaggedStatus()} \n\n # Modified \n${_getModifiedStatus()} \n\n# Untracked \n${_getUntrackedStatus()}\n"
  }

  def getLastCommit(): Option[Commit] = {
    val hash = FilesIO.getHash(s"${repoDir}${File.separator}HEAD")
    hash.map(value => Commit.getCommit(value))
  }

  def _getStaggedStatus(): String = "Not yet implemented"

  def _getModifiedStatus(): String = "Not yet implemented"

  def _getUntrackedStatus(): String = {
    val files: Array[String] = getLastCommit() match {
      case None         => allFiles
      case Some(commit) => allFiles.diff(commit.tree.getAllFiles())
    }

    files.mkString("\n")
  }

  def allFiles: Array[String] =
    FilesIO.getAllFiles(s"${repoDir}${File.separator}..")
}

object Repo {
  def init(path: String): String = {
    val sgitDir = s"${path}${File.separator}.sgit${File.separator}"
    val dirs: Array[String] = Array(
      s"${sgitDir}tags",
      s"${sgitDir}commits",
      s"${sgitDir}trees",
      s"${sgitDir}blobs",
      s"${sgitDir}branches"
    )
    val files: Array[String] = Array(s"${sgitDir}STAGE", s"${sgitDir}HEAD");
    if (!FilesIO.dirExists(sgitDir)) {
      try {
        FilesIO.createDirectories(dirs);
        FilesIO.createFiles(files);
        return "Repo initialized"
      } catch {
        case e: Exception => e.getMessage();
      }
    } else {
      "Sorry, a sgit repository is already initialized";
    }
  }

  def getRepoDir(): Option[String] = {
    return FilesIO.getRepoDirPath(".sgit")
  }
}
