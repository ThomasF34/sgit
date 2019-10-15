package igpolytech

case class Branch(name: String, commitHash: String) {
  def getLastCommit(commitsPath: String): Option[Commit] = {
    if (commitHash != "") Some(Commit.getCommit(commitHash, commitsPath))
    else None
  }

  def update(newCommitHash: String): Branch = Branch(name, newCommitHash)

  def save(branchesPath: String) = {
    FilesIO.write(s"${branchesPath}$name", commitHash)
  }
}

object Branch {
  def getBranchOption(name: String, branchesPath: String): Option[Branch] =
    if (branchExists(name, branchesPath))
      Some(fromBranchName(name, branchesPath))
    else None
  def fromBranchName(name: String, branchesPath: String): Branch = {
    Branch(name, FilesIO.getContent(s"${branchesPath}$name"))
  }

  def branchExists(name: String, branchesPath: String): Boolean =
    FilesIO.fileExists(s"${branchesPath}$name")
}
