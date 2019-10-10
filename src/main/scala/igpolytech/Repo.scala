package igpolytech
import java.io.File

case class Repo(repoDir: String) {
  val projectDir = repoDir match {
    case s"${value}.sgit" => value
  }

  val treesPath = s"${repoDir}${File.separator}trees${File.separator}"
  val blobsPath = s"${repoDir}${File.separator}blobs${File.separator}"
  val commitsPath = s"${repoDir}${File.separator}commits${File.separator}"

  def getStatus(): String = {
    s"# Stagged \n${getStaggedStatus()} \n\n# Modified \n${getModifiedStatus()} \n\n# Untracked \n${getUntrackedStatus()}\n"
  }

  def getLastCommit(): Option[Commit] = {
    val hashOption = FilesIO.getHash(s"${repoDir}${File.separator}HEAD")
    hashOption.map(hash => Commit.getCommit(hash, commitsPath))
  }

  private def getStaggedStatus(): String = {
    val hash = FilesIO.getHash(s"${repoDir}${File.separator}STAGE")
    hash match {
      case None => "-- Nothing in stage --"
      case Some(treeHash) => {
        val tree: Tree =
          Tree.getTree(
            treesPath,
            blobsPath,
            treeHash
          )
        tree.getAllFiles().mkString("\n")
      }
    }
  }

  private def getModifiedStatus(): String = {
    getStage() match {
      case Some(stage) => {
        val diff = stage.getModified(projectDir, blobsPath)
        if (diff.isEmpty) "-- Nothing modified --"
        else diff.mkString
      }
      case None => "-- Nothing modified --"
    }
  }

  private def getUntrackedStatus(): String = {
    val files: Array[String] = getStage() match {
      case None        => allFiles
      case Some(stage) => allFiles.diff(stage.getAllFiles())
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
    getStage() match {
      case Some(stage) => setStage(stage.mergeTree(volatileTree))
      case None        => setStage(volatileTree)
    }
    "Changes added"
  }

  def commit(text: String, author: String): String = {
    def commitTree(stage: Tree, lastCommitHash: String): String = {
      val commit = Commit(stage.hash, lastCommitHash, text, author)
      commit.save(commitsPath)
      s"Change commited : ${commit.title}"
    }

    (getStage(), getLastCommit()) match {
      case (Some(stage), Some(commit)) => {
        val tree = Tree.getTree(treesPath, blobsPath, commit.treeHash)
        Diff.fromTrees(stage, tree) match {
          case None        => "Nothing to commit"
          case Some(value) => commitTree(stage, tree.hash)
        }
      }
      case (Some(stage), None) => commitTree(stage, "")
      case (None, _)           => "Nothing in stage"
    }
  }

  def getStage(): Option[Tree] = {
    val stageHashOption = FilesIO.getHash(s"${repoDir}${File.separator}STAGE")
    stageHashOption.map(
      stageHash =>
        Tree.getTree(
          treesPath,
          blobsPath,
          stageHash
        )
    )
  };

  def setStage(newStage: Tree) = {
    newStage.save(
      treesPath,
      blobsPath
    )
    FilesIO.write(s"${repoDir}${File.separator}STAGE", newStage.hash)
  }

  def allFiles: Array[String] =
    FilesIO.getAllFilesPath(projectDir)
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
