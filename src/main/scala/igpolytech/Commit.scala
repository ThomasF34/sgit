package igpolytech

case class Commit(tree: Tree) {}

object Commit {
  def getCommit(hash: String): Commit = {
    //TODO
    new Commit(new Tree("a", "test"));
  }
}
