package diners

import scala.concurrent.Future
import scala.util.{Failure, Success}
import org.scalatest.matchers.MustMatchers

object TestHelper extends MustMatchers {
  def assertState(state:Future[Any], expectedValue: String) : Unit = {
    state.value.get match {
      case Success(s) => { s must be === expectedValue  }
      case Failure(e) => throw e
    }
  }
}
