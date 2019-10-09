package igpolytech
import scopt.OParser

case class Config(
    mode: String = "",
    path: String = ".",
    files: Array[String] = Array(),
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
        .text(
          "creates a new sgit repository in the current directory or in the given path if any"
        )
        .children(
          arg[String]("<path>")
            .optional()
            .action((x, c) => c.copy(path = x))
        ),
      cmd("add")
        .action((_, c) => c.copy(mode = "add"))
        .text("adds the given files to the stage")
        .children(
          arg[String]("<file>...")
            .unbounded()
            .required()
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
          opt[Unit]('v', "verbose")
            .action((_, c) => c.copy(verbose = true))
            .text("show hash and commit subject line for each branch's head"),
          checkConfig(
            c =>
              if (c.displayAll && c.givenName != "")
                failure("'all' option does not make sense with a branch name")
              else if (c.verbose && c.givenName != "")
                failure(
                  "'verbose' option does not make sense with a branch name"
                )
              else success
          )
        ),
      cmd("tag")
        .action((_, c) => c.copy(mode = "tag"))
        .text("TODO"),
      cmd("checkout")
        .action((_, c) => c.copy(mode = "checkout"))
        .text("TODO"),
      cmd("merge")
        .action((_, c) => c.copy(mode = "merge"))
        .text("TODO"),
      cmd("rebase")
        .action((_, c) => c.copy(mode = "rebase"))
        .text("TODO")
    )
  }

// OParser.parse returns Option[Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) => {

      config.mode match {
        case "init" => {
          println(Repo.init(config.path))
        }
        case _ => {
          Repo.getRepoDir() match {
            case None => {
              println(
                "fatal: not a sgit repository (or any of the parent directories): .sgit"
              )
              System.exit(1)
            }
            case Some(value) => {
              val repo: Repo = new Repo(value)
              config.mode match {
                case "add"      => println(repo.add(config.files))
                case "commit"   => println("Not yet implemented")
                case "branch"   => println("Not yet implemented")
                case "log"      => println("Not yet implemented")
                case "diff"     => println("Not yet implemented")
                case "tag"      => println("Not yet implemented")
                case "status"   => println(repo.getStatus())
                case "merge"    => println("Not yet implemented")
                case "rebase"   => println("Not yet implemented")
                case "checkout" => println("Not yet implemented")
              }
            }
          }
        }
      }
    }
    case _ =>
    // arguments are bad, error message will have been displayed
  }

}
