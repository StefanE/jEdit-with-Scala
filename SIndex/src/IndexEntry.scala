package sindex

import java.io.Writer
import org.gjt.sp.jedit.jEdit

object IndexEntry {
  val CLAZZ = 0
  val TRAIT = 1
  val OBJECT = 2
  val CONSTRUCTOR = 3
  val METHOD = 4
  val FIELD = 5
}

class IndexEntry(val entryType: Int,val docName: String,val sourceName: String,val libEntryNum: Int) {
  var next:IndexEntry = null

  override def toString: String = "DocName:" + docName + " SourceName" + sourceName

  def typeToString(t: Int) = t match {
    case IndexEntry.CLAZZ => "Class"
    case IndexEntry.TRAIT => "Trait"
    case IndexEntry.OBJECT => "Object"
    case IndexEntry.CONSTRUCTOR => "Constructor"
    case IndexEntry.METHOD => "Method"
    case IndexEntry.FIELD => "Field"
    case t => "Unknown type:" + t
  }

  def getCompleteURL: String = {
    var url: String = jEdit.getProperty("sindex.lib.doc." + libEntryNum)
    var open_bracket_pos: Int = 0
    var last_dot: Int = 0
    var before: String = null
    var after: String = null
    var before_dot: String = null
    var after_dot: String = null
    entryType match {
      case IndexEntry.METHOD =>
        open_bracket_pos = docName.indexOf('(')
        before = docName.substring(0, open_bracket_pos)
        last_dot = before.lastIndexOf('.')
        before_dot = before.substring(0, last_dot)
        after_dot = docName.substring(last_dot + 1)
        url += changeToPath(before_dot) + ".html#" + after_dot
      case IndexEntry.FIELD =>
        last_dot = docName.lastIndexOf('.')
        before = docName.substring(0, last_dot)
        after = docName.substring(last_dot + 1)
        url += changeToPath(before) + ".html#" + after
      case IndexEntry.OBJECT =>
        url += changeToPath(docName) + ".html"
      case IndexEntry.CONSTRUCTOR =>
        open_bracket_pos = docName.indexOf('(')
        before = docName.substring(0, open_bracket_pos)
        after = docName.substring(open_bracket_pos)
        last_dot = before.lastIndexOf('.')
        before_dot = before.substring(0, last_dot + 1)
        after_dot = before.substring(last_dot + 1)
        url += changeToPath(before_dot) + after_dot + ".html#" + after_dot + after
      //Class or Trait
      case _ =>
        url += changeToPath(docName) + ".html"
    }
    return url
  }

  /**
   * on JavaDoc 1.2 and higher, dots in the package specifications
   * are changed to directory separators (ie. '/' in URLs).
   * If the current libEntry is for old JavaDoc 1.1, then the string
   * is returned unchanged, because package docs are not stored in subdirs
   * on JavaDoc 1.1.
   */
  private def changeToPath(s: String): String = {
    val oldjdoc: String = jEdit.getProperty("sindex.lib.oldjdoc." + libEntryNum)
    if ("true" == oldjdoc) s else s.replace('.', '/')
  }

  /**for debugging purposes only */
  def toStringDebug = typeToString(entryType) + ": " + docName + " [" + libEntryNum + "]"

  def writeXML(w: Writer): Unit = {
    w.write("type: ")
    w.write(entryType.toString)
    w.write(" libNum: ")
    w.write(libEntryNum.toString)
    w.write(" text: ")
    w.write(docName)
  }
}