val x = List(1)

x match {
  case Nil => println("hell")
  case head :: tail => println(s"$head--> $tail")
}