package igpolytech
import java.io.File
import scala.xml.Node

/**
  * This class represent the Head of the sgit repository. It can either be in branch or detached mode.
  * If in branch mode, then the content will be the branch name
  * If in detached mode, then the content will be the commit hash
  */
case class Head(mode: String, content: String) {
  def save(saveHeadToRepo: (Node) => Unit) =
    saveHeadToRepo(toXml())
  // TODO OLD FilesIO.saveXml(this.toXml(), headPath)

  /**
    * This function will return a new Head.
    * If in branch mode : This head is the same but the branch had been saved with newCommitHash
    * If in detached mode : This head will have the new commit hash as content
    */
  def update(
      newContent: String,
      branchContent: (String) => String,
      saveBranchToRepo: (String, String) => Unit
  ): Head = {
    mode match {
      case "branch" => {
        Branch
          .fromBranchName(content, branchContent)
          .update(newContent)
          .save(saveBranchToRepo)
        this
      }
      case "detached" => Head(mode, newContent)
    }
  }

  /**
    * Will return last commit, either from the branch name (if in branch mode) or the commit correponding to the commit we are detached on
    */
  def getLastCommit(
      commitContent: (String) => Node,
      branchContent: (String) => String
  ): Option[Commit] = {
    //TODO DELETE ME
    // val commitContent = (hash: String) =>
    //   FilesIO.loadXml(s"${commitsPath}${hash}")

    mode match {
      case "branch" =>
        Branch
          .fromBranchName(content, branchContent)
          .getLastCommit(commitContent)
      case "detached" => {
        if (content == "") None
        else Some(Commit.getCommit(content, commitContent))
      }
    }
  }

  def toXml(): Node = <Head mode={mode}>{content}</Head>
}

object Head {
  def exists(
      branchName: String,
      branchExists: (String) => Boolean
  ): Boolean =
    branchExists(branchName)
  //TODO old FilesIO.fileExists(s"${branchesPath}$branchName")

  def fromCommitHash(commitHash: String): Head =
    Head("detached", commitHash)

  def fromBranchName(
      branchName: String,
      branchExists: (String) => Boolean
  ): Option[Head] = {
    if (exists(branchName, branchExists)) {
      Some(Head("branch", branchName))
    } else None
  }

  def fromHeadFile(
      headContent: () => Node
  ): Head = {
    // TODO old
    // val xml = FilesIO.loadXml(headPath)
    val xml = headContent()
    val mode = (xml \@ "mode")
    val content = (xml).text
    Head(mode, content)
  }
}
