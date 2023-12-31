package fpinscala.exercises.parsing.working

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:

  def jsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*

    def token(s: String) = string(s).token

    def jArray: Parser[JSON] = (
      token("[")
        *> value.sep(token(",")).map(vs => JArray(vs.toIndexedSeq))
        <* token("]")
    ).scope("array")

    def jObject: Parser[JSON] = (
      token("{")
        *> jKeyValue.sep(token(",")).map(kvs => JObject(kvs.toMap))
        <* token("}")
    ).scope("object")

    def jKeyValue: Parser[(String, JSON)] =
      (jString <* token(":")).slice ** value

    def jNull: Parser[JSON] =
      token("null").as(JNull)

    def jNumber: Parser[JSON] =
      double.map(JNumber(_))

    def jString: Parser[JSON] = 
      quotedString.token.map(JString(_))

    def jBool: Parser[JSON] =
      string("true").as(JBool(true)) | string("false").as(JBool(false))

    def lit: Parser[JSON] = (
      jNull | jNumber | jString | jBool
    ).scope("literal")

    def value: Parser[JSON] = lit | jObject | jArray

    whitespace *> (jArray | jObject).root
