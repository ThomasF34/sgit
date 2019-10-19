package igpolytech
import java.io.File
import scala.xml.Node

case class Repo(repoDir: String)(implicit ioManager: IOManager) {
  val treesPath = s"${repoDir}${File.separator}trees${File.separator}"
  val blobsPath = s"${repoDir}${File.separator}blobs${File.separator}"
  val commitsPath = s"${repoDir}${File.separator}commits${File.separator}"
  val branchesPath = s"${repoDir}${File.separator}branches${File.separator}"
  val tagsPath = s"${repoDir}${File.separator}tags${File.separator}"

  val headDir = s"${repoDir}${File.separator}"
  val headFile = "HEAD"

  val stageDir = s"${repoDir}${File.separator}"
  val stageFile = "STAGE"

  val commitMessageDir = s"${repoDir}${File.separator}"
  val commitMessageFile = "CommitMessage"

  val projectDir = repoDir match {
    case s"${value}.sgit" => value
  }

  lazy val branchContent = ioManager.getContent(branchesPath)(_)
  lazy val blobContent = ioManager.getContent(blobsPath)(_)
  lazy val tagContent = ioManager.getContent(tagsPath)(_)
  lazy val fileContent = (dirName: String) =>
    (fileName: String) =>
      ioManager.getContent(s"${projectDir}$dirName")(fileName)

  lazy val commitContent = ioManager.loadXml(commitsPath)(_)
  lazy val treeContent = ioManager.loadXml(treesPath)(_)
  lazy val headContent = ioManager.loadXml(headDir)(headFile)

  def tagExists = ioManager.fileExists(tagsPath)(_)
  def commitExists = ioManager.fileExists(commitsPath)(_)
  def branchExists = ioManager.fileExists(branchesPath)(_)
  def blobExists(treeName: String)(blobName: String) =
    ioManager.fileExists(projectDir)(s"${treeName}$blobName")

  def saveCommitToRepo = ioManager.saveXml(commitsPath)(_, _)
  def saveTreeToRepo = ioManager.saveXml(treesPath)(_, _)
  def saveHeadToRepo = ioManager.saveXml(headDir)(_, headFile)

  def saveBranchToRepo = ioManager.write(branchesPath)(_, _)
  def saveTagToRepo = ioManager.write(tagsPath)(_, _)
  def saveBlobToRepo = ioManager.write(blobsPath)(_, _)
  def writeBlobFromFile(
      tree: String
  )(blob: String, content: String) =
    ioManager.write(s"${projectDir}${tree}")(blob, content)

  def head: Head = Head.fromHeadFile(headContent)

  def getStatus(): String = {
    s"${getHeadStatus()}\n# Stagged \n${getStaggedStatus()} \n\n# Modified \n${getModifiedStatus()} \n\n# Untracked \n${getUntrackedStatus()}\n"
  }

  def getHeadStatus(): String = {
    head.mode match {
      case "branch" => s"You're on branch ${head.content}"
      case "detached" =>
        s"${Console.RED}${Console.BOLD}!! CAREFUL !! You're on detached mode !${Console.RESET}\nThe commit title you're on is '${Console.BOLD}${Commit
          .getCommit(head.content, commitContent)
          .title}${Console.RESET}'"
    }
  }

  def getLastCommit(): Option[Commit] =
    head.getLastCommit(commitContent, branchContent)

  def setLastCommit(newCommitHash: String) = {
    head
      .update(newCommitHash, branchContent, saveBranchToRepo)
      .save(saveHeadToRepo)
  }

  def setHead(headMode: String, commitHash: String) = {
    Head(headMode, commitHash).save(saveHeadToRepo)
  }

  private def getStaggedStatus(): String = {
    val hash = ioManager.getHash(s"${repoDir}${File.separator}STAGE")
    hash match {
      case None => "-- Nothing in stage --"
      case Some(treeHash) => {
        val stageTree: Tree =
          Tree.getTree(
            treeHash,
            blobContent,
            treeContent
          )
        getLastCommit() match {
          case Some(commit) => {
            val commitTree =
              Tree.getTree(
                commit.treeHash,
                blobContent,
                treeContent
              )
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
        val diff =
          stage.getModified(
            blobExists,
            blobContent,
            fileContent
          )
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
    val allFiles = (f: File) => ioManager.getAllFiles(f)

    val volatileTree: Tree = Tree.createFromList(
      files.filterNot(_.contains(".sgit")),
      projectDir,
      allFiles,
      fileContent
    )

    // If no stage -> volatile tree = stage (means no commit had been done)
    // If stage -> merge volatile tree with stage tree
    // WARNING : WHEN COMMIT IS DONE WE DO NOT RESET STAGE.
    getStage() match {
      case Some(stage) => setStage(stage.mergeTree(volatileTree))
      case None        => setStage(volatileTree)
    }
    "Changes added"
  }

  def commit(message: String) = {

    def checkMessageAndCommitTree(
        stage: Tree,
        lastCommitHash: String
    ): String = {
      //Before commiting a tree we need to ask for a commit text
      if (message != "")
        commitWithMessage(
          message,
          "NO AUTOR FOR NOW TODO",
          stage,
          lastCommitHash
        )
      else {
        val commitTextPath = s"${commitMessageDir}$commitMessageFile"
        if (ioManager.fileExists(commitMessageDir)(commitMessageFile)) {
          val commitMessage =
            ioManager.getContent(commitMessageDir)(commitMessageFile)
          ioManager.delete(commitTextPath)
          commitWithMessage(
            commitMessage,
            "NO AUTOR FOR NOW TODO",
            stage,
            lastCommitHash
          )
        } else {
          ioManager.createFiles(Array(commitTextPath))
          s"Please edit the commit message file with the commit message and use the commit command again \n Commit Message file is located at ${commitTextPath}"
        }
      }
    }

    def commitWithMessage(
        text: String,
        author: String,
        stage: Tree,
        lastCommitHash: String
    ): String = {
      val commit = Commit(stage.hash, lastCommitHash, text, author)
      commit.save(saveCommitToRepo)
      setLastCommit(commit.hash)
      s"Change commited : ${commit.title}"
    }

    //Check if there is something to commit then check message and commit
    (getStage(), getLastCommit()) match {
      case (Some(stage), Some(commit)) => {
        val tree =
          Tree.getTree(
            commit.treeHash,
            blobContent,
            treeContent
          )
        if (Diff.fromTrees(tree, stage).isEmpty)
          "Sorry, nothing is to be commited. Use sgit add before commiting"
        else checkMessageAndCommitTree(stage, commit.hash)
      }
      case (Some(stage), None) => checkMessageAndCommitTree(stage, "")
      case (None, _) =>
        "Sorry, nothing is to be commited. Use sgit add before commiting"
    }
  }

  def getStage(): Option[Tree] = {
    val stageHashOption = ioManager.getHash(s"${repoDir}${File.separator}STAGE")
    stageHashOption.map(
      stageHash =>
        Tree.getTree(
          stageHash,
          blobContent,
          treeContent
        )
    )
  };

  def setStage(newStage: Tree) = {
    newStage.save(
      saveTreeToRepo,
      saveBlobToRepo
    )
    ioManager.write(stageDir)(stageFile, newStage.hash)
  }

  def allFiles: Array[String] =
    ioManager.getAllFilesPath(projectDir)

  def listBranch(all: Boolean, verbose: Boolean): String = {
    ioManager.getAllFilesPath(branchesPath).mkString
  }

  def createBranch(branchName: String): String = {
    if (branchExists(branchName))
      "Sorry branch name is already used"
    else {
      saveBranchToRepo(branchName, getLastCommit() match {
        case None         => ""
        case Some(commit) => commit.hash
      })
      s"Branch $branchName created"
    }
  }

  def listTags(): String = {
    val tags =
      Tag.allTags(ioManager.getAllFilesPath(tagsPath), tagContent)
    if (!tags.isEmpty) s"Tags:\n - ${tags.mkString("\n - ")}"
    else "No tags created. See sgit tag <name> to create one"
  }

  def createTag(tagName: String): String = {
    if (Tag.exists(tagName, tagExists))
      "Sorry tag name is already used"
    else {
      head.getLastCommit(commitContent, branchContent) match {
        case None =>
          s"The head your repository does not point towards a commit. Please commit before creating a tag"
        case Some(commit) => {
          Tag(tagName, commit.hash).save(saveTagToRepo)
          s"Tag $tagName created"
        }
      }
    }
  }

  def diff(): String = {
    getStage() match {
      case None => "No diff"
      case Some(stage) =>
        val modified = stage
          .getModified(
            blobExists,
            blobContent,
            fileContent
          )

        if (modified.isEmpty) "No diff"
        else modified.map(_.getDetails).mkString("\n")
    }
  }

  def log(withStats: Boolean, withDiff: Boolean): String = {
    def loop(commit: Commit, detailList: Array[String]): Array[String] = {
      if (commit.parentHash == "")
        detailList :+ commit.getDetails(
          withStats,
          withDiff,
          blobContent,
          treeContent,
          commitContent
        )
      else
        loop(
          Commit.getCommit(commit.parentHash, commitContent),
          detailList :+ commit
            .getDetails(
              withStats,
              withDiff,
              blobContent,
              treeContent,
              commitContent
            )
        )
    }

    val headCommit =
      head.getLastCommit(commitContent, branchContent)
    headCommit match {
      case Some(commit) => loop(commit, Array()).mkString("\n")
      case None         => "No commit"
    }
  }

  def checkout(to: String): String = {
    //First checking if there's anything modified
    getStage() match {
      case Some(stage) => {
        val lastCommitTree =
          Tree.getTree(
            getLastCommit().get.treeHash,
            blobContent,
            treeContent
          )
        if (stage
              .getModified(
                blobExists,
                blobContent,
                fileContent
              )
              .isEmpty && Diff.fromTrees(lastCommitTree, stage).isEmpty) {
          //Only then we checkout
          val toCommitOption =
            Commit.getCommitOption(to, commitContent, commitExists)
          val commitWithHeadMode = toCommitOption
            .map(commit => (commit, "detached"))
            .orElse(
              Branch
                .getBranchOption(to, branchExists, branchContent)
                .flatMap(
                  _.getLastCommit(commitContent)
                    .map(commit => (commit, to))
                )
            )
            .orElse(
              Tag
                .fromName(to, tagExists, tagContent)
                .flatMap(_.getCommit(commitContent, commitExists))
                .map(tagCommit => (tagCommit, "detached"))
            )

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
    val destinationTree =
      Tree.getTree(
        commit.treeHash,
        blobContent,
        treeContent
      )
    val destinationFiles =
      destinationTree.getAllFiles()
    val untracked = getUntracked()
    val diff = untracked.intersect(destinationFiles)
    if (!diff.isEmpty)
      s"fatal: The following untracked working tree files would be overwritten by checkout: \n${diff
        .mkString("\n")}\nPlease move or remove them before you switch branches. ${Console.RED}${Console.BOLD}Aborting${Console.RESET}"
    else {
      ioManager.deleteFiles(actualStage.getAllFiles())
      destinationTree.createAllFiles(writeBlobFromFile)
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

  def merge(branchName: String): String = {
    if (head.mode != "branch") "You must be on a branch to merge a branch"
    Branch.getBranchOption(branchName, branchExists, branchContent) match {
      case None => "Sorry, given branch was not founded"
      case Some(branch) =>
        (branch.getLastCommit(commitContent), getLastCommit()) match {
          case (Some(branchCommit), Some(currentCommit)) =>
            Merge.fromCommits(
              branchCommit,
              currentCommit,
              commitContent,
              commitExists
            )
          case (None, Some(currentCommit)) =>
            "The branch you're trying to merge has no commit"
          case (Some(branchCommit), None) =>
            "The branch you're on has no commit"
          case (None, None) =>
            "You cannot merge anything right after initializing your repo"
        }
    }
  }
}

object Repo {
  implicit val ioManager: IOManager = IOManager();

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
    if (!ioManager.dirExists(sgitDir)) {
      try {
        ioManager.createDirectories(dirs);
        ioManager.createFiles(files);
        ioManager.write(s"${sgitDir}branches${File.separator}")("master", "")

        val initialBranchExists =
          ioManager.fileExists(s"${sgitDir}branches${File.separator}")(
            _: String
          )
        val initialSaveHeadToRepo = (xml: Node) =>
          ioManager.saveXml(sgitDir)(xml, "HEAD")
        Head
          .fromBranchName(
            "master",
            initialBranchExists
          )
          .get
          .save(initialSaveHeadToRepo)
        return "Repo initialized"
      } catch {
        case e: Exception => e.getMessage();
      }
    } else {
      "Sorry, a sgit repository is already initialized";
    }
  }

  def getRepoDir(): Option[Repo] =
    ioManager.getRepoDirPath(".sgit").map(sgitDir => Repo(sgitDir))
}
