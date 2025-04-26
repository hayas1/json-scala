import org.scalatest.funsuite.AnyFunSuite

class TokenTest extends AnyFunSuite:
  test("read mat chars") {
    val input = """
      |123
      |45
      |6
      |""".stripMargin

    val tokens = RowColIterator(input.linesIterator.map(_.toList).toList)

    assert(tokens.position == Position(0, 0))
    assert(tokens.peek == '\n')
    assert(tokens.position == Position(0, 0))
    assert(tokens.next() == '\n')
    assert(tokens.hasNext)

    assert(tokens.position == Position(1, 0))
    assert(tokens.peek == '1')
    assert(tokens.next() == '1')
    assert(tokens.next() == '2')
    assert(tokens.next() == '3')
    assert(tokens.position == Position(1, 3))
    assert(tokens.next() == '\n')
    assert(tokens.hasNext)

    assert(tokens.position == Position(2, 0))
    assert(tokens.next() == '4')
    assert(tokens.next() == '5')
    assert(tokens.position == Position(2, 2))
    assert(tokens.next() == '\n')
    assert(tokens.hasNext)

    assert(tokens.position == Position(3, 0))
    assert(tokens.next() == '6')
    assert(tokens.position == Position(3, 1))
    assert(tokens.next() == '\n')
    assert(!tokens.hasNext)
  }
