package jsonstream

import java.io.{InputStreamReader, InputStream}

object Test {
  def main(args: Array[String]) {
    val inputStream: InputStream = getClass.getResourceAsStream("/test.json")
    val reader: InputStreamReader = new InputStreamReader(inputStream)
    val value = parse(reader)
    value match {
      case JsonArray(fields) => {
        println(fields.head)
        println(fields.tail.head)
      }
    }
  }
}
