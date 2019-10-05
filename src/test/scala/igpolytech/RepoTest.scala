package igpolytech

import java.io.File
import org.scalatest._

class RepoTest extends FunSpec {
  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File("../.sgit").exists()) FilesIO.delete("../.sgit")
      if (new File(".sgit").exists()) FilesIO.delete(".sgit")
    }
  }

  describe("Repo") {
    describe("Init") {
      it("should create dirs and files after init") {
        val res = new Repo().init(".")
        assert(res == "Repo initialized")
        assert(new File(".sgit/STAGE").exists())
        assert(new File(".sgit/HEAD").exists())
        assert(new File(".sgit/tags").exists())
        assert(new File(".sgit/commits").exists())
        assert(new File(".sgit/trees").exists())
        assert(new File(".sgit/blobs").exists())
        assert(new File(".sgit/branches").exists())
      }

      it("shouldn't be initialized if already exists in current directory") {
        new Repo().init(".")
        val res = new Repo().init(".")
        assert(res == "Sorry, a sgit repository is already initialized")
      }

      it("shouldn't be initialized if already exists in upper directory") {
        new Repo().init("..")
        val res = new Repo().init(".")
        assert(res == "Sorry, a sgit repository is already initialized")
      }
    }
  }
}
