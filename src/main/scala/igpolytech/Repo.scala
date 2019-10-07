package igpolytech
import java.io.File

class Repo(repoDir: String) {
  val projectDir = repoDir match {
    case s"${value}.sgit" => value
  }
  def getStatus(): String = {
    s"# Stagged \n${_getStaggedStatus()} \n\n # Modified \n${_getModifiedStatus()} \n\n# Untracked \n${_getUntrackedStatus()}\n"
  }

  def getLastCommit(): Option[Commit] = {
    val hash = FilesIO.getHash(s"${repoDir}${File.separator}HEAD")
    hash.map(value => Commit.getCommit(value))
  }

  def _getStaggedStatus(): String = {
    val hash = FilesIO.getHash(s"${repoDir}${File.separator}STAGE")
    hash match {
      case None => "-- Nothing in stage --"
      case Some(treeHash) => {
        val tree: Tree = Tree.getTree(treeHash)
        tree.getAllFiles().mkString("\n")
      }
    }
  }

  def _getModifiedStatus(): String = "Not yet implemented"

  def _getUntrackedStatus(): String = {
    val files: Array[String] = getLastCommit() match {
      case None         => allFiles
      case Some(commit) => allFiles.diff(commit.tree.getAllFiles())
    }

    if (files.isEmpty) "-- Nothing is untracked --"
    else files.mkString("\n")
  }

  def add(files: Array[String]): String = {
    println(s"Repo numero $repoDir")
    println(s"Project nume $projectDir")
    val volatileTree: Tree = Tree.createFromList(
      files.filterNot(_.contains(".sgit")),
      projectDir
    )
    "Not yet implemented"
  }

  def addFile(path: String): String = {
    s"Will add $path"
  }

  def addFiles(dirPath: String): String = {
    s"Will add ${FilesIO.getAllFilesPath(dirPath).mkString(" - ")}"
  }

  def allFiles: Array[String] =
    FilesIO.getAllFilesPath(s"${repoDir}${File.separator}..")
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
