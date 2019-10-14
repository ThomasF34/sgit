package igpolytech
import java.io.File

case class Repo(repoDir: String) {
  val projectDir = repoDir match {
    case s"${value}.sgit" => value
  }

  val treesPath = s"${repoDir}${File.separator}trees${File.separator}"
  val blobsPath = s"${repoDir}${File.separator}blobs${File.separator}"
  val commitsPath = s"${repoDir}${File.separator}commits${File.separator}"
  val headPath = s"${repoDir}${File.separator}HEAD"

  def getStatus(): String = {
    s"# Stagged \n${getStaggedStatus()} \n\n# Modified \n${getModifiedStatus()} \n\n# Untracked \n${getUntrackedStatus()}\n"
  }

  def getLastCommit(): Option[Commit] = {
    val hashOption = FilesIO.getHash(headPath)
    hashOption.map(hash => Commit.getCommit(hash, commitsPath))
  }

  def setLastCommit(commitHash: String) = {
    FilesIO.write(headPath, commitHash)
  }

  private def getStaggedStatus(): String = {
    val hash = FilesIO.getHash(s"${repoDir}${File.separator}STAGE")
    hash match {
      case None => "-- Nothing in stage --"
      case Some(treeHash) => {
        val stageTree: Tree =
          Tree.getTree(
            treesPath,
            blobsPath,
            treeHash
          )
        getLastCommit() match {
          case Some(commit) => {
            val commitTree = Tree.getTree(treesPath, blobsPath, commit.treeHash)
            val diff = Diff.fromTrees(commitTree, stageTree)
            if (diff.isEmpty) "-- Nothing in stage --"
            else diff.mkString("\n")
          }
          case None => {
            Diff.getAddedFromTree(stageTree).mkString("\n")
          }
        }
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

  def commit() = {

    def checkMessageAndCommitTree(
        stage: Tree,
        lastCommitHash: String
    ): String = {
      //Before commiting a tree we need to ask for a commit text
      val commitTextPath = s"${repoDir}${File.separator}CommitMessage"
      if (FilesIO.fileExists(commitTextPath)) {
        val commitMessage = FilesIO.getContent(commitTextPath)
        FilesIO.delete(commitTextPath)
        commitWithMessage(
          commitMessage,
          "NO AUTOR FOR NOW TODO",
          stage,
          lastCommitHash
        )
      } else {
        FilesIO.createFiles(Array(commitTextPath))
        s"Please edit the commit message file with the commit message and use the commit command again \n Commit Message file is located at ${commitTextPath}"
      }
    }

    def commitWithMessage(
        text: String,
        author: String,
        stage: Tree,
        lastCommitHash: String
    ): String = {
      val commit = Commit(stage.hash, lastCommitHash, text, author)
      commit.save(commitsPath)
      setLastCommit(commit.hash)
      s"Change commited : ${commit.title}"
    }

    //Check if there is something to commit then check message and commit
    (getStage(), getLastCommit()) match {
      case (Some(stage), Some(commit)) => {
        val tree = Tree.getTree(treesPath, blobsPath, commit.treeHash)
        if (Diff.fromTrees(stage, tree).isEmpty)
          "Sorry, nothing is to be commited. Use sgit add before commiting"
        else checkMessageAndCommitTree(stage, tree.hash)
      }
      case (Some(stage), None) => checkMessageAndCommitTree(stage, "")
      case (None, _) =>
        "Sorry, nothing is to be commited. Use sgit add before commiting"
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
