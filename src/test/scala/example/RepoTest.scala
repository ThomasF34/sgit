package example
import java.nio.file.Files
import java.nio.file.Paths
import org.scalatest._

class RepoTest extends FlatSpec with Matchers {
  "Dirs and Files" should "be created after init" in {
    new Repo().init()
    Files.exists(Paths.get(".sgit/STAGE")) shouldEqual true
  }
}
