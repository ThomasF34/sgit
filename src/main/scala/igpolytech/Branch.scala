package igpolytech
import scala.xml.Node

case class Branch(name: String, commitHash: String) {
  def getLastCommit(
      commitContent: (String) => Node
  ): Option[Commit] = {
    if (commitHash != "") Some(Commit.getCommit(commitHash, commitContent))
    else None
  }

  def update(newCommitHash: String): Branch = Branch(name, newCommitHash)

  def save(saveBranchToRepo: (String, String) => Unit) =
    saveBranchToRepo(name, commitHash)
}

object Branch {
  def getBranchOption(
      name: String,
      branchExists: (String) => Boolean,
      branchContent: (String) => String
  ): Option[Branch] =
    if (exists(name, branchExists))
      Some(fromBranchName(name, branchContent))
    else None
  def fromBranchName(
      name: String,
      branchContent: (String) => String
  ): Branch =
    Branch(name, branchContent(name))

  def exists(name: String, branchExists: (String) => Boolean): Boolean =
    branchExists(name)
}
