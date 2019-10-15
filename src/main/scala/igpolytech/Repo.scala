package igpolytech
import java.io.File

case class Repo(repoDir: String) {
  val treesPath = s"${repoDir}${File.separator}trees${File.separator}"
  val blobsPath = s"${repoDir}${File.separator}blobs${File.separator}"
  val commitsPath = s"${repoDir}${File.separator}commits${File.separator}"
  val branchesPath = s"${repoDir}${File.separator}branches${File.separator}"
  val tagsPath = s"${repoDir}${File.separator}tags${File.separator}"
  val headPath = s"${repoDir}${File.separator}HEAD"

  def head: Head = Head.fromHeadFile(headPath, commitsPath)
  val projectDir = repoDir match {
    case s"${value}.sgit" => value
  }

  def getStatus(): String = {
    s"${getHeadStatus()}\n# Stagged \n${getStaggedStatus()} \n\n# Modified \n${getModifiedStatus()} \n\n# Untracked \n${getUntrackedStatus()}\n"
  }

  def getHeadStatus(): String = {
    head.mode match {
      case "branch" => s"You're on branch ${head.content}"
      case "detached" =>
        s"${Console.RED}${Console.BOLD}!! CAREFUL !! You're on detached mode !${Console.RESET}\nThe commit title you're on is '${Console.BOLD}${Commit
          .getCommit(head.content, commitsPath)
          .title}${Console.RESET}'"
    }
  }

  def getLastCommit(): Option[Commit] = {
    head.getLastCommit(branchesPath, commitsPath)
  }

  def setLastCommit(newCommitHash: String) = {
    head.update(newCommitHash, branchesPath).save(headPath)
  }

  def setHead(headMode: String, commitHash: String) {
    Head(headMode, commitHash).save(headPath)
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

  def getUntracked(): Array[String] =
    getStage() match {
      case None        => allFiles
      case Some(stage) => allFiles.diff(stage.getAllFiles())
    }
  private def getUntrackedStatus(): String = {
    val files = getUntracked()
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

  def listBranch(all: Boolean, verbose: Boolean): String = {
    FilesIO.getAllFilesPath(branchesPath).mkString
  }

  def createBranch(branchName: String): String = {
    if (FilesIO.fileExists(s"${branchesPath}$branchName"))
      "Sorry branch name is already used"
    else {
      FilesIO.write(s"${branchesPath}$branchName", getLastCommit() match {
        case None         => ""
        case Some(commit) => commit.hash
      })
      s"Branch $branchName created"
    }
  }

  def listTags(): String = {
    val tags = Tag.allTags(tagsPath)
    if (!tags.isEmpty) s"Tags:\n - ${tags.mkString("\n - ")}"
    else "No tags created. See sgit tag <name> to create one"
  }

  def createTag(tagName: String): String = {
    if (Tag.exists(tagName, tagsPath))
      "Sorry tag name is already used"
    else {
      Tag(tagName, FilesIO.getContent(headPath)).save(tagsPath)
      s"Tag $tagName created"
    }
  }

  def checkout(to: String): String = {
    //First checking if there's anything modified
    getStage() match {
      case Some(stage) => {
        val lastCommitTree =
          Tree.getTree(treesPath, blobsPath, getLastCommit().get.treeHash)
        if (stage
              .getModified(projectDir, blobsPath)
              .isEmpty && Diff.fromTrees(lastCommitTree, stage).isEmpty) {
          //Only then we checkout
          val toCommitOption = Commit.getCommitOption(to, commitsPath)
          val commitWithHeadMode = toCommitOption match {
            case Some(commit) => Some((commit, "detached"))
            case None => {
              Branch
                .getBranchOption(to, branchesPath)
                .flatMap(_.getLastCommit(commitsPath)) match {
                case Some(branchCommit) => {
                  Some((branchCommit, to))
                }
                case None => {
                  Tag
                    .fromName(to, tagsPath)
                    .flatMap(_.getCommit(commitsPath))
                    .map(tagCommit => (tagCommit, "detached"))
                }
              }
            }
          }
          commitWithHeadMode match {
            case Some((commit, headMode)) => {
              checkout(stage, commit, headMode)
            }
            case None => "fatal: Cannot find the reference you gave"
          }
        } else {
          "fatal: Please commit your work before checking out other branch/commit/tag"
        }
      }
      case None =>
        "fatal: You cannot checkout just after initializing your sgit repository"
    }
  }

  def checkout(actualStage: Tree, commit: Commit, head: String): String = {
    //Need to check if all actual untracked file are contained in destination commit. If so fatal message
    val destinationTree = Tree.getTree(treesPath, blobsPath, commit.treeHash)
    val destinationFiles =
      destinationTree.getAllFiles()
    val untracked = getUntracked()
    val diff = untracked.intersect(destinationFiles)
    if (!diff.isEmpty)
      s"fatal: The following untracked working tree files would be overwritten by checkout: \n${diff
        .mkString("\n")}\nPlease move or remove them before you switch branches. ${Console.RED}${Console.BOLD}Aborting${Console.RESET}"
    else {
      FilesIO.deleteFiles(actualStage.getAllFiles())
      destinationTree.createAllFiles(projectDir)
      setStage(destinationTree)
      head match {
        case "detached" => {
          setHead(head, commit.hash)
          s"CAREFUL ${Console.RED}You're on detached mode${Console.RESET}\nYou're now at commit ${commit.title}"
        }
        case branchName => {
          setHead("branch", head)
          s"You're now at commit ${commit.title}"
        }
      }
    }
  }
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
        FilesIO.write(s"${sgitDir}branches${File.separator}master", "")
        Head
          .fromBranchName("master", s"${sgitDir}branches${File.separator}")
          .get
          .save(s"${sgitDir}HEAD")
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
