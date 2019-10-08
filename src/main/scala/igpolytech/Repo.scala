package igpolytech
import java.io.File

case class Repo(repoDir: String) {
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
        val tree: Tree =
          Tree.getTree(
            s"${repoDir}${File.separator}trees${File.separator}",
            s"${repoDir}${File.separator}blobs${File.separator}",
            treeHash
          )
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
    val volatileTree: Tree = Tree.createFromList(
      files.filterNot(_.contains(".sgit")),
      projectDir
    )

    volatileTree.toString()

    // If no stage -> volatile tree = stage (means no commit had been done)
    // If stage -> merge volatile tree with stage tree
    //WARNING WHEN COMMIT DO NOT RESET STAGE. WE WONT BE ABLE TO COMPARE WITH STAGE IF RESETED
    getStage match {
      case Some(stage) => setStage(stage.mergeTree(volatileTree))
      case None        => setStage(volatileTree)
    }
    "Changes added"
  }

  def getStage: Option[Tree] = {
    val stageHashOption = FilesIO.getHash(s"${repoDir}${File.separator}STAGE")
    stageHashOption.map(
      stageHash =>
        Tree.getTree(
          s"${repoDir}${File.separator}trees${File.separator}",
          s"${repoDir}${File.separator}blobs${File.separator}",
          stageHash
        )
    )
  };

  def setStage(newStage: Tree) = {
    newStage.save(
      s"${repoDir}${File.separator}trees${File.separator}",
      s"${repoDir}${File.separator}blobs${File.separator}"
    )
    FilesIO.write(s"${repoDir}${File.separator}STAGE", newStage.hash)
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
