package sindex

import org.gjt.sp.jedit.io.VFSManager
import java.util.Vector
import javax.swing.{JCheckBox, JOptionPane}
import java.util.Enumeration
import org.gjt.sp.jedit.{MiscUtilities, jEdit, GUIUtilities}
import org.gjt.sp.util.{Log, WorkRequest}
import java.io._
import com.microstar.xml.XmlException

object SIndexHolder {
  val STATUS_OK = 1
  val STATUS_NOT_EXISTS = 2
  val STATUS_LOAD_ERROR = 3
  val STATUS_INV_FILE = 4
  val STATUS_NO_SETTINGSDIR = 5
  val STATUS_NOT_LOADED = 6
  val STATUS_LOADING = 7
  val STATUS_IS_CREATING = 8
  val STATUS_CREATE_ERROR = 9

  private var index: SIndex = null
  private var listeners: Vector[AnyRef] = new Vector[AnyRef]
  private var indexfilename: String = null
  private var status: Int = 0
  private var isCreatingIndex: Boolean = false
  private val INDEXFILENAME: String = "sindex.ser"


  val homeDir: String = jEdit.getSettingsDirectory

  if (homeDir == null) {
    status = STATUS_NO_SETTINGSDIR
    GUIUtilities.error(null, "sindex.error.nohome", null)
  }
  else {
    var dirname: String = homeDir + File.separator + "sindex"
    var indexdir: File = new File(dirname)
    indexfilename = dirname + File.separator + INDEXFILENAME
    if (!indexdir.exists) {
      indexdir.mkdirs
      status = STATUS_NOT_EXISTS
    }
    else {
      var f: File = new File(indexfilename)
      if (f.exists && f.isFile && f.canRead) status = STATUS_NOT_LOADED
      else status = STATUS_NOT_EXISTS
    }
  }

  def getIndex: SIndex = {
    if (status == SIndexHolder.STATUS_NOT_EXISTS) {
      GUIUtilities.message(null, "sindex.createinfo", null)
      new ConfigureDialog(jEdit.getFirstView)
    }
    else if (status == SIndexHolder.STATUS_NOT_LOADED) {
      VFSManager.runInWorkThread(new SIndexLoader)
    }
    return index
  }

  def getStatus = status

  def getIndexFilename = indexfilename

  def indexExists = status == STATUS_OK || status == STATUS_NOT_LOADED

  def addSIndexListener(listener: SIndexListener) {if (!listeners.contains(listener)) listeners.addElement(listener)}

  def removeSIndexListener(listener: SIndexListener) {if (listeners.contains(listener)) listeners.removeElement(listener)}

  def createIndex(libs: Array[LibEntry]) {
    if (status == STATUS_IS_CREATING) {
      GUIUtilities.message(null, "sindex.createIsRunning", null)
      return
    }
    setStatus(STATUS_IS_CREATING)
    if (!jEdit.getBooleanProperty("options.backgroundinfo.dontShowAgain", false)) {
      var dontShowAgain: JCheckBox = new JCheckBox(jEdit.getProperty("sindex.backgroundinfo.dontShowAgain"), false)
      var title: String = jEdit.getProperty("sindex.backgroundinfo.title")
      var msg: String = jEdit.getProperty("sindex.backgroundinfo.message")
      JOptionPane.showMessageDialog(null, Array[Any](msg, dontShowAgain), title, JOptionPane.INFORMATION_MESSAGE)
      jEdit.setBooleanProperty("options.backgroundinfo.dontShowAgain", dontShowAgain.isSelected)
    }
    VFSManager.runInWorkThread(new SIndexCreator(libs))
  }

  private def setIndex(newIndex: SIndex) {
    synchronized {
      var oldIndex: SIndex = index
      index = newIndex
      if (index != oldIndex) {
        val evt: SIndexChangeEvent = new SIndexChangeEvent(index, status)
        var e = listeners.elements
        while (e.hasMoreElements)
          e.nextElement.asInstanceOf[SIndexListener].indexChanged(evt)
      }
    }
  }

  private def setStatus(newStatus: Int) {
    synchronized {
      var oldStatus: Int = status
      status = newStatus
      if (status != oldStatus) {
        var evt: SIndexChangeEvent = new SIndexChangeEvent(index, status)
        var e: Enumeration[_] = listeners.elements
        while (e.hasMoreElements) (e.nextElement.asInstanceOf[SIndexListener]).indexChanged(evt)
      }
    }
  }

  class SIndexCreator(var libs: Array[LibEntry]) extends WorkRequest {
    def run() {
      var newIndex: SIndex = new SIndex(libs)
      System.gc
      SIndexCreator.this.setStatus("Writing index out to " + getIndexFilename + " ...")
      var fw: FileWriter = null
      var bw: BufferedWriter = null
      // create output stream:
      try {
        fw = new FileWriter(getIndexFilename)
        bw = new BufferedWriter(fw)
      }
      catch {
        case e: IOException => {
          GUIUtilities.error(null, "sindex.error.write", Array[AnyRef](getIndexFilename, e))
          return
        }
      }
      // write index out:
      try {
        newIndex.writeXML(bw)
        bw.flush
        bw.close
        setIndex(newIndex)
        SIndexHolder.this.setStatus(STATUS_OK)
      }
      catch {
        case e: OutOfMemoryError => {
          System.gc
          SIndexHolder.this.setStatus(STATUS_CREATE_ERROR)
          Log.log(Log.ERROR, this, e.toString)
          var mxOption: String = "-Xmx<size>m"
          if (MiscUtilities.compareVersions("1.2", System.getProperty("scala.version")) > 0) mxOption = "-mx<size>m"
          GUIUtilities.error(null, "sindex.error.outofmemory", Array[AnyRef]("creating", mxOption))
        }
        case e: IOException => {
          SIndexHolder.this.setStatus(STATUS_CREATE_ERROR)
          GUIUtilities.error(null, "sindex.error.write", Array[AnyRef](getIndexFilename, e))
        }
      }
      finally {
        newIndex = null
        libs = null
        System.gc
      }
    }
  }

  class SIndexLoader extends WorkRequest {
    class CountReader(val r: Reader) extends BufferedReader(r) {
      override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
        var ret: Int = super.read(cbuf, off, len)
        pos += len
        ({readNr += 1; readNr - 1})
        if (readNr % 30 == 0) setProgressValue(pos.asInstanceOf[Int])
        ret
      }

      override def read(cbuf: Array[Char]): Int = {
        var ret: Int = super.read(cbuf)
        pos += cbuf.length
        ({readNr += 1; readNr - 1})
        if (readNr % 30 == 0) setProgressValue(pos.asInstanceOf[Int])
        ret
      }

      override def read: Int = {
        var ret: Int = super.read
        ({pos += 1; pos - 1})
        ({readNr += 1; readNr - 1})
        if (readNr % 30 == 0) setProgressValue(pos.asInstanceOf[Int])
        ret
      }

      private var readNr: Long = 0
      private var pos: Long = 0
    }
    def run() {
      if (getStatus != STATUS_NOT_LOADED) return
      // inform listeners to display an info text.
      SIndexHolder.setStatus(STATUS_LOADING)
      SIndexLoader.this.setStatus("Loading index...")
      // load the SIndex
      try {
        var f: File = new File(getIndexFilename)
        setProgressMaximum(f.length.asInstanceOf[Int])
        var fr: FileReader = new FileReader(f)
        var cr: CountReader = new CountReader(fr)
        var newIndex: SIndex = new SIndex
        newIndex.readXML(getIndexFilename)
        cr.close
        setIndex(newIndex)
        SIndexHolder.this.setStatus(STATUS_OK)
      }
      catch {
        case e: OutOfMemoryError => {
          System.gc
          SIndexHolder.this.setStatus(STATUS_LOAD_ERROR)
          Log.log(Log.ERROR, this, e.toString)
          var mxOption: String = "-Xmx<size>m"
          if (MiscUtilities.compareVersions("1.2", System.getProperty("scala.version")) > 0) mxOption = "-mx<size>m"
          GUIUtilities.error(null, "sindex.error.outofmemory", Array[AnyRef]("loading", mxOption))
        }
        case fnf: FileNotFoundException => {
          SIndexHolder.this.setStatus(STATUS_NOT_EXISTS)
        }
        case xe: XmlException => {
          SIndexHolder.this.setStatus(STATUS_INV_FILE)
          Log.log(Log.ERROR, this, xe.toString)
          var msg: String = xe.getMessage
          if (msg.startsWith("wrong version")) {
            GUIUtilities.error(null, "sindex.error.wrongversion", Array[AnyRef](getIndexFilename, xe.getMessage))
            new ConfigureDialog(jEdit.getFirstView)
          }
          else {
            GUIUtilities.error(null, "sindex.error.invalidfile", Array[AnyRef](getIndexFilename, xe.getMessage, new Integer(xe.getLine), new Integer(xe.getColumn)))
          }
        }
        case e: Exception => {
          SIndexHolder.this.setStatus(STATUS_LOAD_ERROR)
          Log.log(Log.ERROR, this, e)
          GUIUtilities.error(null, "sindex.error.load", Array[AnyRef](getIndexFilename, e))
        }
      }
    }
  }
}