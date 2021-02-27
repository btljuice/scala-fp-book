package sfpbook.ch9

// A value in JSON can be:
// - dictionary: "{ "key1": value1, ... }"
//   key must be a string
// - array "[ json1, ... ]"

sealed trait Json
object Json {
  case object JNull extends Json
  case class JNumber(get: Double) extends Json
  case class JString(get: String) extends Json
  case class JBoolean(get: Boolean) extends Json
  case class JArray(get: IndexedSeq[Json]) extends Json
  case class JObject(get: Map[String, Json]) extends Json

  def parser[Err, Parser[+_]](P: Parsers[Err,Parser]): Parser[Json] = {
    import P._
    val self: Parser[Json] = lazzy(parser(P))

    def commaSeparated[A](a: => Parser[A]): Parser[List[A]] = sepSequence(spaces ** ',' ** spaces)(a)

    val jnull: Parser[JNull.type] = string("null").map(_ => JNull)
    val jnumber: Parser[JNumber] = double.map(JNumber)
    val jstring: Parser[JString] = dblQuotedString.map(JString)
    val jboolean: Parser[JBoolean] = boolean.map(JBoolean)
    val jarray: Parser[JArray] = separated3(spaces)("[", commaSeparated(self), "]")
      .map { case (_, l, _) => JArray(l.toIndexedSeq) }

    val jobject: Parser[JObject] = {
      val keyValues: Parser[Map[String, Json]] = commaSeparated(
        separated3(spaces)(dblQuotedString, ':', self)
      ).map { _.map { case (k, _, v) => (k, v) }.toMap }

      separated3(spaces)("{", keyValues, "}").map { case (_, m, _) => JObject(m) }
    }

    jnull | jnumber | jstring | jboolean | jarray | jobject
  }
}
