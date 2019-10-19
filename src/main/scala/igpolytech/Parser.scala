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
            .optional()
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
              if (c.mode == "branch" && c.displayAll && c.givenName != "")
                failure("'all' option does not make sense with a branch name")
              else if (c.mode == "branch" && c.verbose && c.givenName != "")
                failure(
                  "'verbose' option does not make sense with a branch name"
                )
              else if (c.mode == "branch" && !c.displayAll && !c.verbose && c.givenName == "")
                failure(
                  "You must either put a name or an option. See sgit --help for help"
                )
              else success
          )
        ),
      cmd("tag")
        .action((_, c) => c.copy(mode = "tag"))
        .text("create a new tag (list if no name is given)")
        .children(
          arg[String]("name")
            .optional()
            .action((x, c) => c.copy(givenName = x))
            .text("name of the tag")
        ),
      cmd("checkout")
        .action((_, c) => c.copy(mode = "checkout"))
        .text(
          "Goes to a given branch, tag or commit. Commit is given as a hash, branch and tag as a name. Commit will be taken in priority then branch then tag"
        )
        .children(
          arg[String]("name")
            .required()
            .action((x, c) => c.copy(givenName = x))
            .text("hash of the commit or name of the branch/tag")
        ),
      cmd("merge")
        .action((_, c) => c.copy(mode = "merge"))
        .text(
          "Will merge given branch into current one. Cannot be done in detached mode"
        )
        .children(
          arg[String]("name")
            .required()
            .action((x, c) => c.copy(givenName = x))
            .text("Branch name")
        ),
      cmd("rebase")
        .action((_, c) => c.copy(mode = "rebase"))
        .text("Not yet impletemented"),
      checkConfig(
        c =>
          if (c.mode == "")
            failure("There were no command given ! Please see usage.")
          else success
      )
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
            case Some(repo) => {
              config.mode match {
                case "add"    => println(repo.add(config.files))
                case "commit" => println(repo.commit())
                case "branch" =>
                  if (config.displayAll || config.verbose)
                    println(repo.listBranch(config.displayAll, config.verbose))
                  else println(repo.createBranch(config.givenName))
                case "log"  => println(repo.log(config.stats, config.patch))
                case "diff" => println(repo.diff())
                case "tag" =>
                  if (config.givenName == "") println(repo.listTags())
                  else println(repo.createTag(config.givenName))
                case "status"   => println(repo.getStatus())
                case "merge"    => println(repo.merge(config.givenName))
                case "rebase"   => println("Not yet implemented")
                case "checkout" => println(repo.checkout(config.givenName))
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
