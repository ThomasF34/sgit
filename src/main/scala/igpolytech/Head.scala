package igpolytech
import java.io.File
import scala.xml.Node

/**
  * This class represent the Head of the sgit repository. It can either be in branch or detached mode.
  * If in branch mode, then the content will be the branch name
  * If in detached mode, then the content will be the commit hash
  */
case class Head(mode: String, content: String) {
  def save(headPath: String) = FilesIO.saveXml(this.toXml(), headPath)

  /**
    * This function will return a new Head.
    * If in branch mode : This head is the same but the branch had been saved with newCommitHash
    * If in detached mode : This head will have the new commit hash as content
    */
  def update(newContent: String, branchesPath: String): Head = {
    mode match {
      case "branch" => {
        Branch
          .fromBranchName(content, branchesPath)
          .update(newContent)
          .save(branchesPath)
        this
      }
      case "detached" => Head(mode, newContent)
    }
  }

  /**
    * Will return last commit, either from the branch name (if in branch mode) or the commit correponding to the commit we are detached on
    */
  def getLastCommit(
      branchesPath: String,
      commitsPath: String
  ): Option[Commit] = {
    mode match {
      case "branch" =>
        Branch
          .fromBranchName(content, branchesPath)
          .getLastCommit(commitsPath)
      case "detached" => {
        if (content == "") None
        else Some(Commit.getCommit(content, commitsPath))
      }
    }
  }

  def toXml(): Node = <Head mode={mode}>{content}</Head>
}

object Head {
  def exists(branchesPath: String, branchName: String): Boolean =
    FilesIO.fileExists(s"${branchesPath}$branchName")

  def fromCommitHash(commitHash: String): Head =
    Head("detached", commitHash)

  def fromBranchName(
      branchName: String,
      branchesPath: String
  ): Option[Head] = {
    if (exists(branchesPath, branchName)) {
      Some(Head("branch", branchName))
    } else None
  }

  def fromHeadFile(headPath: String, commitsPath: String): Head = {
    val xml = FilesIO.loadXml(headPath)
    val mode = (xml \@ "mode")
    val content = (xml).text
    Head(mode, content)
  }
}