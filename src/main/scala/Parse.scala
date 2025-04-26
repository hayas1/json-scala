class Parser(cursor: RowColIterator):
  def parse_value(): Json =
    ControlToken(cursor.dropWhitespace().peek) match
      case Some(LeftBrace) => parse_object()
      case Some(Quote)     => parse_string()
      case _               => Json.ValueNull

  def parse_object() =
    val leftBrace = cursor.dropWhitespace().next()
    if ControlToken(leftBrace) != Some(LeftBrace) then
      throw new RuntimeException(f"expected left brace, but got '$leftBrace'")
    var items = List[(String, Json)]()
    while ControlToken(cursor.peek) != Some(RightBrace) do
      items = items :+ parse_object_item()
      cursor.dropWhitespace()
    val rightBrace = cursor.next()
    if ControlToken(rightBrace) != Some(RightBrace) then
      throw new RuntimeException(f"expected right brace, but got '$rightBrace'")
    Json.ValueObject(items.toMap)

  def parse_object_item() =
    val key = parse_string().asString.get

    val colon = cursor.dropWhitespace().next()
    if ControlToken(colon) != Some(Colon) then
      throw new RuntimeException(f"expected colon, but got '$colon'")

    val value = parse_value()
    (key, value)

  def parse_string() =
    val startQuote = cursor.dropWhitespace().next()
    if ControlToken(startQuote) != Some(Quote) then
      throw new RuntimeException(f"expected quote, but got '$startQuote'")
    val value =
      Json.ValueString(
        cursor.takeWhile(ControlToken(_) != Some(Quote)).mkString
      )
    // val endQuote = cursor.peek
    // if ControlToken(endQuote) != Some(Quote) then
    //   throw new RuntimeException(f"expected quote, but got '$endQuote'")
    value

object Parser:
  def apply(target: String) =
    new Parser(
      new RowColIterator(target.linesIterator.toList.map(_.toList))
    )

  sealed trait ParserError
  case class UnexpectedToken(token: Char) extends ParserError
