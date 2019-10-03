package example
import java.io.File
import scopt.OParser

case class Config(
    mode: String = "",
    files: Seq[File] = Seq(),
    patch: Boolean = false,
    stats: Boolean = false,
    givenName: String = "",
    displayAll: Boolean = false,
    verbose: Boolean = false
)

object Parser extends App {
  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("sgit"),
      head("sgit", "1.x"),
      help("help")
        .text("Here is the usage of sgit"),
      cmd("init")
        .action((_, c) => c.copy(mode = "init"))
        .text("creates a new sgit repository in the current directory"),
      cmd("add")
        .action((_, c) => c.copy(mode = "add"))
        .text("adds the given files to the stage")
        .children(
          arg[File]("<file>...")
            .unbounded()
            .optional()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("file to add to stage")
        ),
      cmd("status")
        .action((_, c) => c.copy(mode = "status"))
        .text(
          "display the status of the repo (untracked, modified, staged files)"
        ),
      cmd("commit")
        .action((_, c) => c.copy(mode = "commit"))
        .text("create a new commit with staged files"),
      cmd("diff")
        .action((_, c) => c.copy(mode = "diff"))
        .text("display the delta of modified files with staged files"),
      cmd("log")
        .action((_, c) => c.copy(mode = "log"))
        .text("display commits information in a chronological orger")
        .children(
          opt[Unit]('p', "patch")
            .text("show the patch (diff) of each commited file ")
            .action((_, c) => c.copy(patch = true)),
          opt[Unit]("stats")
            .text(
              "show the stats of insertion and deletion of each commited file"
            )
            .action((_, c) => c.copy(stats = true))
        ),
      cmd("branch")
        .action((_, c) => c.copy(mode = "branch"))
        .text("create a new branch")
        .children(
          arg[String]("name")
            .required()
            .action((x, c) => c.copy(givenName = x))
            .text("name of the branch"),
          opt[Unit]('a', "all")
            .action((_, c) => c.copy(displayAll = true))
            .text("display all branches"),
            // TODO must validate "do not make sense with a branch name"
          opt[Unit]('v', "verbose")
            .action((_, c) => c.copy(verbose = true))
            .text("show hash and commit subject line for each branch's head"),
            // TODO must validate "do not make sense with a branch name"
        )
    )
  }

// OParser.parse returns Option[Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) => println(config.mode)
    // do something
    case _ =>
    // arguments are bad, error message will have been displayed
  }

}
