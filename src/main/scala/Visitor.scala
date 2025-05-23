import scala.deriving.*
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.collection.Factory
import cats.syntax.traverse.*

trait Visitor[T]:
  def expectType: List[ValueType]
  def visitObject(accessor: ObjectAccessor): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Object))
  def visitArray(accessor: ArrayAccessor): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Array))
  def visitString(string: String): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.String))
  def visitNumber(number: Double): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Number))
  def visitBool(bool: Boolean): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Bool))
  def visitNull(): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Null))

object Visitor:
  inline def derived[T](using m: Mirror.ProductOf[T]): Visitor[T] =
    new Visitor[T]:
      def expectType = List(ValueType.Object)
      override def visitObject(accessor: ObjectAccessor) =
        val typename = constValue[m.MirroredLabel]
        val elemLabels = summonLabels[m.MirroredElemLabels]
        val elemInstances = summonAll[m.MirroredElemTypes]
        readFields(typename, accessor, elemLabels, elemInstances).map(
          m.fromProduct
        )

  private inline def summonAll[Elems <: Tuple]: List[Visitor[?]] =
    inline erasedValue[Elems] match
      case _: (t *: ts)  => summonInline[Visitor[t]] :: summonAll[ts]
      case _: EmptyTuple => Nil

  private inline def summonLabels[Elems <: Tuple]: List[String] =
    inline erasedValue[Elems] match
      case _: (t *: ts) =>
        constValue[t].asInstanceOf[String] :: summonLabels[ts]
      case _: EmptyTuple => Nil

  private def readFields(
      typename: String,
      accessor: ObjectAccessor,
      fieldNames: List[String],
      fieldVisitors: List[Visitor[?]]
  ): Either[VisitorError, Tuple] =
    val visitors = fieldNames.zip(fieldVisitors).toMap
    var valueMap = scala.collection.mutable.Map[String, Any]()

    while accessor.hasNextPair do
      accessor.nextName[String]() match
        case Right(name) if visitors contains name =>
          accessor.nextValue()(using visitors(name)) match
            case Right(value) => valueMap(name) = value
            case Left(err)    => return Left(VisitorError.Parsing(err))
        case Right(other) =>
          return Left(VisitorError.UnexpectedField(typename, other))
        case Left(e) => return Left(VisitorError.Parsing(e))

    for {
      values <- fieldNames.foldLeft(
        Right(Seq.empty[Any]): Either[VisitorError, Seq[Any]]
      ) {
        case (Right(l), name) =>
          for {
            value <- valueMap
              .get(name)
              .toRight(VisitorError.MissingField(typename, name))
          } yield (l :+ value)
        case (left @ Left(_), _) => left
      }
    } yield Tuple.fromArray(values.toArray)

sealed trait VisitorError extends ParseError
object VisitorError:
  case class Custom[E](c: E) extends VisitorError:
    override def cause = c match
      case v: VisitorError => v.cause
      case _               => None
    def message = c.toString
  case class Parsing[E <: ParseError](e: E) extends VisitorError: // TODO remove
    override def cause = Some(e)
    def message = e.message
  case class MissMatchType(exp: List[ValueType], act: ValueType)
      extends VisitorError:
    def message =
      val or = exp.map(_.toString).mkString(" or ")
      f"cannot parse $act as $or"
  case class MissingField(typename: String, field: String) extends VisitorError:
    def message = f"missing field $typename.$field"
  case class UnexpectedField(typename: String, field: String)
      extends VisitorError:
    def message = f"$typename has no field such as $field"

given Visitor[String] with
  def expectType = List(ValueType.String)
  override def visitString(string: String) = Right(string)

given Visitor[Int] with
  def expectType = List(ValueType.Number)
  override def visitNumber(number: Double) = Right(number.toInt)
given Visitor[Long] with
  def expectType = List(ValueType.Number)
  override def visitNumber(number: Double) = Right(number.toLong)
given Visitor[Float] with
  def expectType = List(ValueType.Number)
  override def visitNumber(number: Double) = Right(number.toFloat)
given Visitor[Double] with
  def expectType = List(ValueType.Number)
  override def visitNumber(number: Double) = Right(number)

given [T](using visitor: Visitor[T]): Visitor[Option[T]] with
  def expectType = List(ValueType.Null) ++ visitor.expectType
  override def visitObject(accessor: ObjectAccessor) =
    visitor.visitObject(accessor).map(Some(_))
  override def visitArray(accessor: ArrayAccessor) =
    visitor.visitArray(accessor).map(Some(_))
  override def visitString(string: String) =
    visitor.visitString(string).map(Some(_))
  override def visitNumber(number: Double) =
    visitor.visitNumber(number).map(Some(_))
  override def visitBool(bool: Boolean) =
    visitor.visitBool(bool).map(Some(_))
  override def visitNull() = Right(None)

given [F[_] <: Iterable[?], T](using
    factory: Factory[T, F[T]],
    visitor: Visitor[T]
): Visitor[F[T]] with
  def expectType = List(ValueType.Array)
  override def visitArray(accessor: ArrayAccessor) =
    for {
      elements <- accessor.elements.toList.sequence.left
        .map(VisitorError.Parsing(_))
    } yield factory.fromSpecific(elements)

given [M[_, _] <: Map[?, ?], K, V](using
    factory: Factory[(K, V), M[K, V]],
    kv: Visitor[K],
    vv: Visitor[V]
): Visitor[M[K, V]] with
  def expectType = List(ValueType.Object)
  override def visitObject(accessor: ObjectAccessor) =
    for {
      pairs <- accessor
        .pairs[K, V]
        .toList
        .sequence
        .left
        .map(VisitorError.Parsing(_))
    } yield factory.fromSpecific(pairs.toMap)
