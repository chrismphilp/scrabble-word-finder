
import scalaj.http.{Http, HttpResponse}

import scala.util.matching.Regex

object WordDefinition {

  val NO_DEFINITION: String = "No definitions found."

  def getWordDefinition(word: String): String = {
    val response: HttpResponse[String] = Http("https://api.dictionaryapi.dev/api/v2/entries/en/" + word)
      .timeout(5000, 10000)
      .asString

    val definitionPattern = new Regex("\"definition\":\"(.*?)\"")
    val definitionPatternString = (definitionPattern findFirstMatchIn response.body).mkString(",")
    if (definitionPatternString.length > 13) {
      definitionPatternString.substring(14, definitionPatternString.length - 1)
    } else NO_DEFINITION
  }
}
