import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.blocking
import scala.io.Source
import scala.util.{Failure, Success}

implicit val ec = ExecutionContext.global

val first: Future[Int] = Future {
  val source = Source.fromFile("src/SolutionPart1.sc")
  source.toSeq.indexOfSlice("def")
}

first.onComplete{
  case Success(x) => println(x)
  case Failure(e) => println("error")
}

@volatile var totalA = 0
val text = Future {
  "na" * 16 + "BATMAN!!!"
}
text onSuccess {
  case txt => totalA += txt.count(_ == 'a')
}
text onSuccess {
  case txt => totalA += txt.count(_ == 'A')
}

val f = Future {
  2 / 0
}

Thread.sleep(1000)


for (exc <- f.failed) println(exc)

totalA