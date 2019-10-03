package example

object Hello{
  val repo: Repo = new Repo()
  val action: String = "init"

  action match {
    case "init" => {
      new Repo().init() match {
        case Some(s) => println(s)
        case None => println("Repo initialized")
      }
    }
    case _ => {
      //repo = new Repo().searchRepoDir()
      println("Usage wesh")
    }
  }
}

