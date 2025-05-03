import org.scalatest.funsuite.AnyFunSuite

class SpanTest extends AnyFunSuite:
  test("position to span") {
    val position = Position(10, 10)

    assert(position.asSpan == Span(Position(10, 10), Position(10, 10)))
  }

  test("merge span") {
    val span1 = Span(Position(10, 10), Position(20, 20))
    val span2 = Span(Position(30, 30), Position(40, 40))

    assert(span1.merged(span2) == Span(Position(10, 10), Position(40, 40)))
  }

  test("print 1-indexed span like position") {
    val span = Span(Position(9, 9), Position(9, 9))

    assert(span.toString == "row 10, col 10")
  }

  test("print 1-indexed span") {
    val span = Span(Position(9, 9), Position(19, 19))

    assert(span.toString == "(10, 10) to (20, 20)")
  }
