package sindex

import java.util.zip.{ZipEntry, ZipFile}
import org.gjt.sp.jedit.{jEdit, MiscUtilities}
import java.lang.reflect.{Modifier, Field, Method, Constructor}
import org.gjt.sp.util.{WorkThread, Log}
import java.text.Collator
import java.util.Hashtable
import java.io._
import util.Sorting
import SIndex.SearchResult

object SIndex {
  val VERSION: String = "1.0"
  private var ignoreCaseCollator: Collator = null
  private var indexEntryComparator: IndexEntryCompare = null
  indexEntryComparator = new IndexEntryCompare

  class SearchResult {
    var entries: Array[IndexEntry] = null
    var keywordpos: Int = 0
    var searchstring: String = null
    var keyword: String = null
  }

  class IndexEntryCompare extends Ordering[IndexEntry] {
    def compare(obj1: IndexEntry, obj2: IndexEntry) = obj1.docName.compareToIgnoreCase(obj2.docName)
  }

  /**test if the modifiers fits the properties set by the user */
  private def fitsProperties(visibility: Int, mods: Int) = {
    if (visibility == LibEntry.PRIVATE) true
    else if (visibility == LibEntry.PACKAGE && !Modifier.isPrivate(mods)) true
    else if (visibility == LibEntry.PROTECTED && (Modifier.isProtected(mods) || Modifier.isPublic(mods))) true
    else if (visibility == LibEntry.PUBLIC && Modifier.isPublic(mods)) true
    else false
  }

  private def getParamList(params: Array[Class[_]]): String = {
    var plist: StringBuilder = new StringBuilder("(")
    var i = 0
    while (i < params.length) {
      if (i != 0)
        plist.append(", ")
      plist.append(getTypeName(params(i)))
      i += 1
    }
    plist.append(")")
    plist.toString
  }

  private def getTypeName(c: Class[_]): String = {
    var cod: String = c.getName
    if (c.isArray) {
      var result: String = null
      var arraycnt: Int = 0
      while (cod.charAt(arraycnt) == '[') ({arraycnt += 1; arraycnt})
      cod.charAt(arraycnt) match {
        case 'B' => result = "byte"
        case 'C' => result = "char"
        case 'D' => result = "double"
        case 'F' => result = "float"
        case 'I' => result = "int"
        case 'J' => result = "long"
        case 'S' => result = "short"
        case 'Z' => result = "boolean"
        case 'L' => result = cod.substring(arraycnt + 1, cod.length - 1)
        case _ => ""
      }
      var i: Int = 0
      while (i < arraycnt) {
        result += "[]"
        i += 1
      }
      result
    }
    else cod
  }
}

class SIndex {
  // private members
  private var keys: Array[String] = null
  private var entries: Array[IndexEntry] = null

  def this(libs: Array[LibEntry]) = {
    this ();
    // count the number of files in the libraries (this is an
    // estimate for the number of classes to process).
    var numFiles: Int = 0
    var i = 0
    while (i < libs.length) {
      var f: ZipFile = libs(i).getLibFile
      numFiles += SIndexUtilities.zipFileSize(f)
      i += i
    }
    var allNames: Hashtable[AnyRef, AnyRef] = new Hashtable[AnyRef, AnyRef](numFiles * 10)
    setNumIterations(numFiles + 3)
    // parse all libraries:
    i = 0
    while (i < libs.length) {
      addLib(allNames, i, libs(i))
      i += 1
    }
    // fill and sort arrays 'keys' and 'entries'
    setStatusText("Sorting...")
    var size: Int = allNames.size
    keys = new Array[String](size)
    entries = new Array[IndexEntry](size)
    var e: java.util.Enumeration[AnyRef] = allNames.keys
    i = 0
    while (e.hasMoreElements) {
      keys(i) = e.nextElement.asInstanceOf[String]
      i += 1
    }
    advance
    Sorting.quickSort(keys)
    advance
    i = 0
    while (i < size) {
      entries(i) = allNames.get(keys(i)).asInstanceOf[IndexEntry]
      i += 1
    }
    advance
    // clear the hashtable, it is no longer needed
    allNames.clear
    allNames = null
  }

  def getNumKeywords = if (keys != null) keys.length else 0

  def getKeywordAt(pos: Int): String = if (keys != null) keys(pos) else null

  def getEntriesAt(i: Int): Array[IndexEntry] = {
    if (i < 0) return null
    var count: Int = 1
    var e: IndexEntry = entries(i)
    println("entru number:" + i)
    while (e.next != null) {
      ({count += 1; count})
      e = e.next
    }

    var arr: Array[IndexEntry] = new Array[IndexEntry](count)
    e = entries(i)
    count = 0
    do {
      arr(count) = e
      e = e.next
      ({count += 1; count})
    } while (e != null)
    Sorting.quickSort[IndexEntry](arr)(SIndex.indexEntryComparator)

    return arr
  }

  def search(searchstring: String, exact: Boolean): SearchResult = {
    var keyword: String = null
    var bracketpos: Int = searchstring.indexOf('(')
    if (bracketpos != -1) {
      keyword = searchstring.substring(0, bracketpos)
    }
    else {
      keyword = searchstring
    }
    var lastdotpos: Int = keyword.lastIndexOf('.')
    if (lastdotpos != -1) {
      keyword = keyword.substring(lastdotpos + 1)
    }
    Log.log(Log.DEBUG, this, "keyword = " + keyword)
    var res: SearchResult = new SearchResult
    res.searchstring = searchstring
    res.keyword = keyword
    res.keywordpos = binarySearch(keyword)
    if (res.keywordpos < 0 && exact) {
      res.entries = null
    }
    else {
      res.entries = getEntriesAt(Math.abs(res.keywordpos))
    }
    res
  }

  def writeXML(w: Writer) {
    w.write("SIndex datafile" + "\n")
    w.write("Keys: " + keys.length)
    w.write("\n")
    println("WRITING")
    for (i <- 0 until keys.length)
      {
        w.write("Key: " + keys(i))
        var arr: Array[IndexEntry] = getEntriesAt(i)
        w.write(" entries: " + arr.length + " \n")
        for (j <- 0 until arr.length)
          {
            arr(j).writeXML(w)
            w.write('\n')
          }
      }
    w.write("\n END")
  }

  def readXML(filename: String): Unit = {
    var lastEntry: IndexEntry = null
    var entryLibNum: Int = 0
    var currentAttribute: String = null
    var entryText: String = null
    var entryType: Int = 0
    var currentIndex: Int = 0

    val reader = new BufferedReader(new FileReader(filename))
    while (reader.ready)
      {
        val l: String = reader.readLine
        if (l.startsWith("Key:")) {
          lastEntry = null
          val splitted = l.split(" ")
          val key = splitted(1)
          val entries = splitted(2)
          if (keys.length > (currentIndex + 1)) currentIndex += 1
          keys(currentIndex) = key

        }
        else if (l.startsWith("type"))
          {
            val splitted = l.split(" ")
            val indexType = splitted(1).toInt
            val libNum = splitted(3).toInt
            val text = splitted(5)
            val newEntry = new IndexEntry(indexType, text, text, libNum)
            if (lastEntry == null) entries(currentIndex) = newEntry
            else lastEntry.next = newEntry
            lastEntry = newEntry
          }
        else if (l.startsWith("Keys")) {
          val splitted = l split (" ")
          keys = new Array[String](splitted(1).toInt + 1)
          entries = new Array[IndexEntry](splitted(1).toInt + 1)
          println("SIZE:" + entries.size)
        }
      }
  }

  private def addLib(allNames: Hashtable[_, _], currentLibNum: Int, libEntry: LibEntry) {
    var f: ZipFile = libEntry.getLibFile
    var e: java.util.Enumeration[_ <: ZipEntry] = f.entries
    var classloader: ZIPClassLoader = new ZIPClassLoader(f)
    setStatusText("Processing ...")
    var i = 0
    while (e.hasMoreElements) {
      advance
      var ze: ZipEntry = e.nextElement.asInstanceOf[ZipEntry]
      if (!ze.isDirectory) {
        var name: String = ze.getName
        if (name.endsWith(".class")) {
          var classname: String = MiscUtilities.fileToClass(name)
          try {
            var c: Class[_] = classloader.loadClass(classname, false)
            processClass(allNames, currentLibNum, c, false, libEntry.visibility)
          }
          catch {
            case t: Throwable => {
              Log.log(Log.WARNING, this, t.toString + ": " + name + " (" + f.getName + ")")
            }
          }
        }
      }
      ({
        i += 1;
        i
      })
    }
  }

  private def add(allNames: Hashtable[_, _], indexType: Int, qualifiedName: String, paramList: String, currentLibNum: Int): Unit = {
    val allNamesLocal = allNames.asInstanceOf[Hashtable[AnyRef, AnyRef]]
    var dotpos: Int = qualifiedName.lastIndexOf('.')
    var dollarpos: Int = qualifiedName.lastIndexOf('$')
    var shortBegin: Int = Math.max(dotpos, -1) + 1
    var shortName: String = qualifiedName.substring(shortBegin).toLowerCase
    var fullName: String = qualifiedName + (if (paramList == null) "" else paramList)
    var sourceName: String = SIndexUtilities.scalaNamesConverter(fullName)
    sourceName = sourceName.replace("$", "")
    shortName = SIndexUtilities.scalaNamesConverter(shortName)
    var eOld: IndexEntry = allNamesLocal.get(shortName).asInstanceOf[IndexEntry]
    var eNew: IndexEntry = new IndexEntry(indexType, fullName, sourceName, currentLibNum)
    if (eOld == null) {
      var endPos: Int = shortName.length - 1
      var filtered: String = if ((shortName.lastIndexOf("$") == endPos)) shortName.substring(0, endPos) else shortName
      allNamesLocal.put(filtered.asInstanceOf[AnyRef], eNew.asInstanceOf[AnyRef])
    }
    else {
      eNew.next = eOld.next
      eOld.next = eNew
    }
  }

  private def processClass(
          allNames: Hashtable[_, _], currentLibNum: Int,
          c: Class[_],
          isInnerClass: Boolean,
          visibility: Int): Unit = {
    var mods: Int = c.getModifiers
    if (SIndex.fitsProperties(visibility, mods)) {
      var cname: String = c.getName
      var url: String = jEdit.getProperty("sindex.lib.doc." + currentLibNum)
      var urlPath: String = (url + cname).replace('.', '/') + ".html"
      try {
        var file: File = new File(urlPath.replace("file:/", ""))
        System.out.println("CANREAD:" + urlPath + ":" + file.canRead)
        if (!file.canRead) return
      }
      catch {
        case e: Exception => {
          return
        }
      }
      var indexType: Int = IndexEntry.CLAZZ
      if (cname.endsWith("$")) indexType = IndexEntry.OBJECT else if (c.isInterface) indexType = IndexEntry.TRAIT

      add(allNames, indexType, cname, null, currentLibNum)

      var constr: Array[Constructor[_]] = c.getDeclaredConstructors

      var i: Int = 0
      while (i < constr.length) {
        {
          var cmods: Int = constr(i).getModifiers
          if (SIndex.fitsProperties(visibility, cmods)) {
            add(allNames, IndexEntry.CONSTRUCTOR, constr(i).getName, SIndex.getParamList(constr(i).getParameterTypes), currentLibNum)
          }
        }
        ({
          i += 1;
          i
        })
      }

      var methods: Array[Method] = c.getDeclaredMethods
      i = 0
      while (i < methods.length) {
        {
          var mmods: Int = methods(i).getModifiers
          if (SIndex.fitsProperties(visibility, mmods)) {
            add(allNames, IndexEntry.METHOD, cname + "." + methods(i).getName, SIndex.getParamList(methods(i).getParameterTypes), currentLibNum)
          }
        }
        ({
          i += 1;
          i
        })
      }

      var fields: Array[Field] = c.getDeclaredFields
      i = 0
      while (i < fields.length) {
        {
          var fmods: Int = fields(i).getModifiers
          if (SIndex.fitsProperties(visibility, fmods)) {
            add(allNames, IndexEntry.FIELD, cname + "." + fields(i).getName, null, currentLibNum)
          }
        }
        ({
          i += 1;
          i
        })
      }

      var classes: Array[Class[_]] = c.getDeclaredClasses
      i = 0
      while (i < classes.length) {
        processClass(allNames, currentLibNum, classes(i), true, visibility)
        ({
          i += 1;
          i
        })
      }
    }
  }

  /**binary search for the keyword */
  private def binarySearch(keyword: String): Int = {
    var beg: Int = 0
    var end: Int = keys.length - 1
    while (beg <= end) {
      var mid: Int = (beg + end) / 2
      var res: Int = keys(mid).compareToIgnoreCase(keyword)
      if (res < 0) beg = mid + 1
      else if (res > 0) end = mid - 1
      else return mid
    }
    -beg
  }

  private def advance {
    var thread: Thread = Thread.currentThread
    if (thread.isInstanceOf[WorkThread]) {
      var w: WorkThread = thread.asInstanceOf[WorkThread]
      var value: Int = w.getProgressValue
      w.setProgressValue(value + 1)
    }
  }

  private def setNumIterations(num: Int) {
    var thread: Thread = Thread.currentThread
    if (thread.isInstanceOf[WorkThread]) (thread.asInstanceOf[WorkThread]).setProgressMaximum(num)
  }

  private def setStatusText(statusText: String) {
    var thread: Thread = Thread.currentThread
    if (thread.isInstanceOf[WorkThread]) (thread.asInstanceOf[WorkThread]).setStatus(statusText)
  }
}