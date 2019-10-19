package igpolytech
import java.io.File
import scala.xml.Node

case class Repo(repoDir: String) {
  val treesPath = s"${repoDir}${File.separator}trees${File.separator}"
  val blobsPath = s"${repoDir}${File.separator}blobs${File.separator}"
  val commitsPath = s"${repoDir}${File.separator}commits${File.separator}"
  val branchesPath = s"${repoDir}${File.separator}branches${File.separator}"
  val tagsPath = s"${repoDir}${File.separator}tags${File.separator}"
  val headPath = s"${repoDir}${File.separator}HEAD"

  // TODO delete me
  val headContent = () => FilesIO.loadXml(headPath)

  def head: Head = Head.fromHeadFile(headContent)
  val projectDir = repoDir match {
    case s"${value}.sgit" => value
  }

  def getStatus(): String = {
    s"${getHeadStatus()}\n# Stagged \n${getStaggedStatus()} \n\n# Modified \n${getModifiedStatus()} \n\n# Untracked \n${getUntrackedStatus()}\n"
  }

  def getHeadStatus(): String = {
    //TODO DELETE ME
    val commitContent = (hash: String) =>
      FilesIO.loadXml(s"${commitsPath}${hash}")
    head.mode match {
      case "branch" => s"You're on branch ${head.content}"
      case "detached" =>
        s"${Console.RED}${Console.BOLD}!! CAREFUL !! You're on detached mode !${Console.RESET}\nThe commit title you're on is '${Console.BOLD}${Commit
          .getCommit(head.content, commitContent)
          .title}${Console.RESET}'"
    }
  }

  def getLastCommit(): Option[Commit] = {
    //TODO DELETE ME
    val commitContent = (hash: String) =>
      FilesIO.loadXml(s"${commitsPath}${hash}")
    head.getLastCommit(branchesPath, commitsPath, commitContent)
  }

  def setLastCommit(newCommitHash: String) = {
    val saveHeadToRepo = (xml: Node) => FilesIO.saveXml(xml, headPath)
    head.update(newCommitHash, branchesPath).save(saveHeadToRepo)
  }

  def setHead(headMode: String, commitHash: String) = {
    val saveHeadToRepo = (xml: Node) => FilesIO.saveXml(xml, headPath)
    Head(headMode, commitHash).save(saveHeadToRepo)
  }

  private def getStaggedStatus(): String = {
    //TODO DELETE ME
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${blobsPath}$blobHash")
    val treeContent = (treeHash: String) =>
      FilesIO.loadXml(s"${treesPath}$treeHash")

    val hash = FilesIO.getHash(s"${repoDir}${File.separator}STAGE")
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
        //TODO Delete me
        val blobExists = (treeName: String) =>
          (blobName: String) =>
            FilesIO.fileExists(s"${projectDir}${treeName}${blobName}")
        val fileContent = (treeName: String) =>
          (blobName: String) =>
            FilesIO.getContent(s"${projectDir}${treeName}${blobName}")
        val blobContent = (blobHash: String) =>
          FilesIO.getContent(s"${blobsPath}${blobHash}")
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
    //TODO DELETE ME
    val allFiles = (f: File) => FilesIO.getAllFiles(f)
    val fileContent = (dirName: String) =>
      (fileName: String) =>
        FilesIO.getContent(s"${projectDir}${dirName}$fileName")

    val volatileTree: Tree = Tree.createFromList(
      files.filterNot(_.contains(".sgit")),
      projectDir,
      allFiles,
      fileContent
    )

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
      //TODO DELETE ME
      val saveCommitToRepo = (hash: String, xml: Node) =>
        FilesIO.saveXml(xml, s"${commitsPath}${hash}")

      val commit = Commit(stage.hash, lastCommitHash, text, author)
      commit.save(saveCommitToRepo)
      setLastCommit(commit.hash)
      s"Change commited : ${commit.title}"
    }

    //Check if there is something to commit then check message and commit
    (getStage(), getLastCommit()) match {
      case (Some(stage), Some(commit)) => {
        //TODO DELETE ME
        val blobContent = (blobHash: String) =>
          FilesIO.getContent(s"${blobsPath}$blobHash")
        val treeContent = (treeHash: String) =>
          FilesIO.loadXml(s"${treesPath}$treeHash")
        val tree =
          Tree.getTree(
            commit.treeHash,
            blobContent,
            treeContent
          )
        if (Diff.fromTrees(stage, tree).isEmpty)
          "Sorry, nothing is to be commited. Use sgit add before commiting"
        else checkMessageAndCommitTree(stage, commit.hash)
      }
      case (Some(stage), None) => checkMessageAndCommitTree(stage, "")
      case (None, _) =>
        "Sorry, nothing is to be commited. Use sgit add before commiting"
    }
  }

  def getStage(): Option[Tree] = {
    //TODO DELETE ME
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${blobsPath}$blobHash")
    val treeContent = (treeHash: String) =>
      FilesIO.loadXml(s"${treesPath}$treeHash")

    val stageHashOption = FilesIO.getHash(s"${repoDir}${File.separator}STAGE")
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
    //TODO
    val writeBlobToRepo = (content: String, hash: String) =>
      FilesIO.write(s"${blobsPath}${hash}", content)
    //TODO
    val saveTreeAsXml = (xml: Node, hash: String) =>
      FilesIO.saveXml(xml, s"${treesPath}${hash}")
    newStage.save(
      saveTreeAsXml,
      writeBlobToRepo
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
    //TODO Delete me
    val tagContent = (name: String) => FilesIO.getContent(s"${tagsPath}$name")
    val tags =
      Tag.allTags(FilesIO.getAllFilesPath(tagsPath), tagContent)
    if (!tags.isEmpty) s"Tags:\n - ${tags.mkString("\n - ")}"
    else "No tags created. See sgit tag <name> to create one"
  }

  def createTag(tagName: String): String = {
    //TODO DELETE ME
    val tagExists = (name: String) => FilesIO.fileExists(s"${tagsPath}$name")

    if (Tag.exists(tagName, tagExists))
      "Sorry tag name is already used"
    else {
      //TODO delete me
      val saveTagToRepo = (name: String, hash: String) =>
        FilesIO.write(s"${tagsPath}$name", hash)

      Tag(tagName, FilesIO.getContent(headPath)).save(saveTagToRepo)
      s"Tag $tagName created"
    }
  }

  def diff(): String = {
    // TODO DELETE ME
    val blobExists = (treeName: String) =>
      (blobName: String) =>
        FilesIO.fileExists(s"${projectDir}${treeName}${blobName}")
    def vlov(treename: String)(blobName: String) = FilesIO.fileExists(treesPath)
    val fileContent = (treeName: String) =>
      (blobName: String) =>
        FilesIO.getContent(s"${projectDir}${treeName}${blobName}")
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${blobsPath}${blobHash}")

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
    //TODO Delete me
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${blobsPath}$blobHash")
    val treeContent = (treeHash: String) =>
      FilesIO.loadXml(s"${treesPath}$treeHash")
    //TODO DELETE ME
    val commitContent = (hash: String) =>
      FilesIO.loadXml(s"${commitsPath}${hash}")

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
      head.getLastCommit(branchesPath, commitsPath, commitContent)
    headCommit match {
      case Some(commit) => loop(commit, Array()).mkString("\n")
      case None         => "No commit"
    }
  }

  def checkout(to: String): String = {
    // TODO Delete me
    val blobExists = (treeName: String) =>
      (blobName: String) =>
        FilesIO.fileExists(s"${projectDir}${treeName}${blobName}")
    val fileContent = (treeName: String) =>
      (blobName: String) =>
        FilesIO.getContent(s"${projectDir}${treeName}${blobName}")
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${blobsPath}${blobHash}")
    val treeContent = (treeHash: String) =>
      FilesIO.loadXml(s"${treesPath}$treeHash")
    val commitContent = (hash: String) =>
      FilesIO.loadXml(s"${commitsPath}${hash}")
    val commitExists = (hash: String) =>
      FilesIO.fileExists(s"${commitsPath}$hash")
    val tagExists = (name: String) => FilesIO.fileExists(s"${tagsPath}$name")
    val tagContent = (name: String) => FilesIO.getContent(s"${tagsPath}$name")

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
                .getBranchOption(to, branchesPath)
                .flatMap(
                  _.getLastCommit(commitsPath)
                    .map(commit => (commit, to))
                )
            )
            .orElse(
              Tag
                .fromName(to, tagExists, tagContent)
                .flatMap(_.getCommit(commitContent, commitExists))
                .map(tagCommit => (tagCommit, "detached"))
            )
          // val commitWithHeadMode = toCommitOption match {
          //   case Some(commit) => Some((commit, "detached"))
          //   case None => {
          //     Branch
          //       .getBranchOption(to, branchesPath)
          //       .flatMap(_.getLastCommit(commitsPath)) match {
          //       case Some(branchCommit) => {
          //         Some((branchCommit, to))
          //       }
          //       case None => {
          //         Tag
          //           .fromName(to, tagsPath)
          //           .flatMap(_.getCommit(commitsPath))
          //           .map(tagCommit => (tagCommit, "detached"))
          //       }
          //     }
          //   }
          // }
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
    //TODO DELETE ME
    val blobContent = (blobHash: String) =>
      FilesIO.getContent(s"${blobsPath}$blobHash")
    val treeContent = (treeHash: String) =>
      FilesIO.loadXml(s"${treesPath}$treeHash")

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
      FilesIO.deleteFiles(actualStage.getAllFiles())
      //TODO DELETE ME
      def writeBlobToRepo(nameTree: String)(nameBlob: String, content: String) =
        FilesIO.write(s"${projectDir}${nameTree}$nameBlob", content)
      destinationTree.createAllFiles(writeBlobToRepo)
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
    //TODO DELETE ME
    val commitContent = (hash: String) =>
      FilesIO.loadXml(s"${commitsPath}${hash}")
    val commitExists = (hash: String) =>
      FilesIO.fileExists(s"${commitsPath}$hash")

    if (head.mode != "branch") "You must be on a branch to merge a branch"
    Branch.getBranchOption(branchName, branchesPath) match {
      case None => "Sorry, given branch was not founded"
      case Some(branch) =>
        (branch.getLastCommit(commitsPath), getLastCommit()) match {
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

        //TODO DELETE ME
        val branchExists = (branchName: String) =>
          FilesIO.fileExists(s"${sgitDir}branches${File.separator}$branchName")
        val saveHeadToRepo = (xml: Node) =>
          FilesIO.saveXml(xml, s"${sgitDir}HEAD")
        Head
          .fromBranchName(
            "master",
            branchExists
          )
          .get
          .save(saveHeadToRepo)
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
