package igpolytech
import org.scalatest.FunSpec
import org.scalatest.Matchers
import java.io.File

class CommitTest extends FunSpec with Matchers {
  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File(".sgit").exists()) FilesIO.delete(".sgit")
    }
  }

  describe("Commit creation") {
    it("shouldn't get commit from empty HEAD") {
      Repo.init(".")
      val repo = Repo(".sgit")

      val res = repo.getLastCommit()

      assert(res.isEmpty)
    }

    it("should ask for completion of CommitMessage file") {
      Repo.init(".")
      val repo = Repo(".sgit")
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob = new Blob("A113", getFakeContent)
      val tree = new Tree("", Array(), Array(blob))
      repo.setStage(tree)

      val res = repo.commit()

      assert(
        res.contains(
          "Please edit the commit message file with the commit message and use the commit command again"
        )
      )
    }

    it("should change HEAD branches when commiting") {
      Repo.init(".")
      val repo = Repo(".sgit")
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob = new Blob("A113", getFakeContent)
      val tree = new Tree("", Array(), Array(blob))
      repo.setStage(tree)
      FilesIO.write(".sgit/")("CommitMessage", "John Lasseter is great")

      val res = repo.commit()

      val commitOption = repo.getLastCommit()
      val branchName = repo.head.content
      val commitOptionFromBranch = Branch
        .fromBranchName(branchName, repo.branchContent)
        .getLastCommit(repo.commitContent)
      assert(commitOptionFromBranch.isDefined)
      assert(commitOption.isDefined)
      assert(
        res.contains(
          "Change commited : John Lasseter is great"
        )
      )
      commitOption.get.text shouldBe "John Lasseter is great"
      commitOptionFromBranch.get.hash shouldBe commitOption.get.hash
    }

    it("should not try to commit if nothing is to be commited") {
      Repo.init(".")
      val repo = Repo(".sgit")

      val res = repo.commit()

      res shouldBe "Sorry, nothing is to be commited. Use sgit add before commiting"

    }
  }
}
