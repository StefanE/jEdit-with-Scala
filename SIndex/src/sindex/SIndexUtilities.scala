package sindex

object SIndexUtilities {
  def scalaNamesConverter(name: String): String = {
    var sourceCodeName: String = name
    sourceCodeName = sourceCodeName.replace("$eq", "=")
    sourceCodeName = sourceCodeName.replace("$greater", "<")
    sourceCodeName = sourceCodeName.replace("$less", ">")
    sourceCodeName = sourceCodeName.replace("$plus", "+")
    sourceCodeName = sourceCodeName.replace("$minus", "-")
    sourceCodeName = sourceCodeName.replace("$times", "*")
    sourceCodeName = sourceCodeName.replace("$slash", "/")
    sourceCodeName = sourceCodeName.replace("$bang", "!")
    sourceCodeName = sourceCodeName.replace("$hash", "#")
    sourceCodeName = sourceCodeName.replace("$percent", "%")
    sourceCodeName = sourceCodeName.replace("$up", "^")
    sourceCodeName = sourceCodeName.replace("$amp", "&")
    sourceCodeName = sourceCodeName.replace("$tilde", "~")
    sourceCodeName = sourceCodeName.replace("$qmark", "?")
    sourceCodeName = sourceCodeName.replace("$bar", "|")
    sourceCodeName = sourceCodeName.replace("$bslash", "\\")
    sourceCodeName = sourceCodeName.replace("$colon", ":")

    sourceCodeName
  }
}