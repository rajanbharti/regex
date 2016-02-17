import org.scalatest.{FunSuite, Matchers}

class TextParserSpec extends FunSuite with Matchers {
  val textParser = new TextParser

  test("concat test") {
    textParser.parseText("ababa", "ab") should be("0 2")
    textParser.parseText("sbaba", "ab") should be("2")
  }

  test("union test") {
    textParser.parseText("abcbd", "a|b") should be("0 1 3")
  }

  test("kleene test") {
    textParser.parseText("", "") should be()
  }


}
