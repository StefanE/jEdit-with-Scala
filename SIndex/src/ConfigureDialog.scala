package sindex

import org.gjt.sp.jedit.gui.EnhancedDialog
import javax.swing._
import java.io.File
import org.gjt.sp.jedit.{GUIUtilities, jEdit}
import java.awt.event.{ActionEvent, ActionListener}
import java.awt._
import table.{AbstractTableModel, TableCellRenderer, DefaultTableCellRenderer, TableColumn}
import java.net.{MalformedURLException, URL}

import javax.swing._
import javax.swing.filechooser._
import javax.swing.table._

object ConfigureDialog {
  private var chooser: JFileChooser = null

  protected val colNorm: Color = UIManager.getColor("Table.background")
  protected val colNormSel: Color = UIManager.getColor("Table.selectionBackground")
  protected val colDis: Color = UIManager.getColor("Label.background")
  protected val colDisSel: Color = colDis.darker
}


class ConfigureDialog(parent: Frame)
        extends EnhancedDialog(parent, jEdit.getProperty("options.sindex.createIndex"), true) {
  private var bAdd: JButton = null
  private var bDel: JButton = null
  private var bOk: JButton = null
  private var bCancel: JButton = null
  private var bRecreate: JButton = null
  private var apiTable: JTable = null
  private var apiModel: ApiTableModel = null

  def init = {
    // create library table
    var scrollTable: JScrollPane = createLibraryTable
    // create "Add"/"Remove" library buttons
    var boxLibButtons: JPanel = new JPanel(new BorderLayout(5, 5))
    bAdd = new JButton(jEdit.getProperty("options.sindex.addLibEntry"))
    boxLibButtons.add(bAdd, BorderLayout.NORTH)
    var boxLibButtons2: JPanel = new JPanel(new BorderLayout(5, 5))
    bDel = new JButton(jEdit.getProperty("options.sindex.delLibEntry"))
    boxLibButtons2.add(bDel, BorderLayout.NORTH)
    boxLibButtons.add(boxLibButtons2, BorderLayout.CENTER)
    // create library panel
    var boxLib: Box = Box.createHorizontalBox
    boxLib.add(scrollTable)
    boxLib.add(Box.createHorizontalStrut(10))
    boxLib.add(boxLibButtons)
    // create "Ok"/"Cancel"/"Recreate" buttons
    var boxButtons: Box = Box.createHorizontalBox
    boxButtons.add(Box.createHorizontalGlue)
    bOk = new JButton(jEdit.getProperty("options.sindex.ok"))
    boxButtons.add(bOk)
    boxButtons.add(Box.createHorizontalStrut(5))
    bCancel = new JButton(jEdit.getProperty("options.sindex.cancel"))
    boxButtons.add(bCancel)
    boxButtons.add(Box.createHorizontalStrut(5))
    bRecreate = new JButton(jEdit.getProperty("options.sindex.recreate"))
    boxButtons.add(bRecreate)
    boxButtons.add(Box.createHorizontalGlue)
    // complete layout
    var stage: JPanel = new JPanel(new BorderLayout(10, 10))
    stage.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10))
    stage.add(new JLabel(jEdit.getProperty("options.sindex.table.label")), BorderLayout.NORTH)
    stage.add(boxLib, BorderLayout.CENTER)
    stage.add(boxButtons, BorderLayout.SOUTH)
    getContentPane.add(stage)
    var ah: ActionHandler = new ActionHandler
    bOk.addActionListener(ah)
    bCancel.addActionListener(ah)
    bRecreate.addActionListener(ah)
    bAdd.addActionListener(ah)
    bDel.addActionListener(ah)
    pack
    if (parent != null) setLocationRelativeTo(parent)
    setVisible(true)
  }

  private def createLibraryTable: JScrollPane = {
    apiModel = new ApiTableModel
    apiTable = new JTable(apiModel)
    apiTable.getTableHeader.setReorderingAllowed(false)
    apiTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    apiTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
    apiTable.setPreferredScrollableViewportSize(new Dimension(630, 200))
    apiTable.setDefaultRenderer(classOf[String], new EditableTableCellRenderer)
    apiTable.getColumnModel.getColumn(0).setPreferredWidth(200)
    apiTable.getColumnModel.getColumn(1).setPreferredWidth(200)
    apiTable.getColumnModel.getColumn(2).setPreferredWidth(80)
    apiTable.getColumnModel.getColumn(3).setPreferredWidth(140)
    var visibilities: Array[String] = Array[String](jEdit.getProperty("options.sindex.table.public"), jEdit.getProperty("options.sindex.table.protected"), jEdit.getProperty("options.sindex.table.package"), jEdit.getProperty("options.sindex.table.private"))
    var col3: TableColumn = apiTable.getColumnModel.getColumn(3)
    var combo: ComboCellRenderer = new ComboCellRenderer(visibilities)
    combo.setRequestFocusEnabled(false)
    col3.setCellRenderer(combo)
    combo = new ComboCellRenderer(visibilities)
    combo.setRequestFocusEnabled(false)
    col3.setCellEditor(new DefaultCellEditor(combo))
    apiTable.setRowHeight(combo.getPreferredSize.height)
    new JScrollPane(apiTable)
  }

  /**
   * create/update index file
   */
  private def createIndex: Unit = {
    if (SIndexHolder.indexExists) {
      var result: Int = JOptionPane.showConfirmDialog(null, jEdit.getProperty("options.sindex.error.overwrite.message", Array[AnyRef](SIndexHolder.getIndexFilename)), jEdit.getProperty("options.sindex.error.overwrite.title"), JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE)
      if (result != JOptionPane.YES_OPTION) return
    }
    apiModel.save
    SIndexHolder.createIndex(apiModel.getLibs)
  }

  private def createFileChooser: Unit = {
    if (ConfigureDialog.chooser != null) return
    ConfigureDialog.chooser = new JFileChooser
    ConfigureDialog.chooser.setDialogTitle(jEdit.getProperty("options.sindex.addLibEntry.title"))
    ConfigureDialog.chooser.setDialogType(JFileChooser.OPEN_DIALOG)
    ConfigureDialog.chooser.setFileSelectionMode(JFileChooser.FILES_ONLY)
    ConfigureDialog.chooser.addChoosableFileFilter(new FileFilter {
      def accept(f: File): Boolean = {
        return f.isDirectory || f.getName.toLowerCase.endsWith(".jar") || f.getName.toLowerCase.endsWith(".zip")
      }

      def getDescription: String = {
        return jEdit.getProperty("options.sindex.addLibEntry.filefilter")
      }
    })
  }

  private def addEntry: Unit = {
    createFileChooser
    var retVal: Int = ConfigureDialog.chooser.showOpenDialog(ConfigureDialog.this)
    if (retVal == JFileChooser.APPROVE_OPTION) {
      var f: File = ConfigureDialog.chooser.getSelectedFile
      var fname: String = f.getAbsolutePath
      if (apiModel.exists(fname)) {
        GUIUtilities.error(null, "options.sindex.error.libexists", null)
      }
      else {
        var doc: String = GUIUtilities.input(null, "options.sindex.inputDoc", "file:/")
        if (doc != null) {
          if (doc.charAt(doc.length - 1) != '/') doc += '/'
          apiModel.add(new LibEntry(fname, doc, false, LibEntry.PROTECTED))
        }
      }
    }
  }

  def ok: Unit = {
    if (apiModel.entriesOk) {
      var status: Int = SIndexHolder.getStatus
      if (apiModel.libsChanged || status == SIndexHolder.STATUS_NOT_EXISTS || status == SIndexHolder.STATUS_LOAD_ERROR) createIndex
      else apiModel.save
      setVisible(false)
      dispose
    }
  }

  def cancel: Unit = {
    setVisible(false)
    dispose
  }

  class ActionHandler extends ActionListener {
    def actionPerformed(evt: ActionEvent): Unit = {
      var button: JButton = evt.getSource.asInstanceOf[JButton]
      if (button == bAdd) {
        addEntry
      }
      else if (button == bDel) {
        var rows = apiTable.getSelectedRows
        if (rows.length == 0) GUIUtilities.error(null, "options.sindex.error.noselection", null)
        else {
          var i: Int = rows.length - 1
          while (i >= 0) {
            apiModel.delete(rows(i))
            ({i -= 1; i})
          }
        }
      }
      else if (button == bOk) ok
      else if (button == bRecreate && apiModel.entriesOk) createIndex
      else if (button == bCancel) cancel
    }
  }

  private class EditableTableCellRenderer extends DefaultTableCellRenderer {
    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, col: Int): Component = {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col)
      setOpaque(true)
      if (table.isCellEditable(row, col)) setBackground(if (isSelected) ConfigureDialog.colNormSel else ConfigureDialog.colNorm)
      else setBackground(if (isSelected) ConfigureDialog.colDisSel else ConfigureDialog.colDis)
      this
    }
  }

  private class ApiTableModel extends AbstractTableModel {
    private var libs = new java.util.Vector[LibEntry]
    var libsChanged = false
    init

    def init {
      var count: Int = 0
      // load libs from properties:
      do {
        var lname: String = jEdit.getProperty("sindex.lib.name." + count)
        if (lname == null || lname.length == 0) return
        var ldoc: String = jEdit.getProperty("sindex.lib.doc." + count)
        var isOldJavaDoc: Boolean = "true".equals(jEdit.getProperty("sindex.lib.oldjdoc." + count))
        var vis: String = jEdit.getProperty("sindex.lib.visibility." + count)
        add(new LibEntry(lname, ldoc, isOldJavaDoc, string2vis(vis)))
        ({count += 1; count})
      } while (true)
    }

    private def string2vis(vis: String): Int = {
      var value = LibEntry.PROTECTED
      if (jEdit.getProperty("options.sindex.table.public").equals(vis)) value = LibEntry.PUBLIC
      else if (jEdit.getProperty("options.sindex.table.package").equals(vis)) value = LibEntry.PACKAGE
      else if (jEdit.getProperty("options.sindex.table.private").equals(vis)) value = LibEntry.PRIVATE
      return value
    }

    private def vis2string(vis: Int): String = {
      vis match {
        case LibEntry.PUBLIC => jEdit.getProperty("options.sindex.table.public")
        case LibEntry.PACKAGE => jEdit.getProperty("options.sindex.table.package")
        case LibEntry.PRIVATE => jEdit.getProperty("options.sindex.table.private")
        case _ => jEdit.getProperty("options.sindex.table.protected")
      }
    }

    def add(l: LibEntry): Unit = {
      libs.addElement(l)
      libsChanged = true
      fireTableRowsInserted(libs.size - 1, libs.size - 1)
    }

    def delete(row: Int): Unit = {
      libs.removeElementAt(row)
      libsChanged = true
      fireTableRowsDeleted(row, row)
    }


    def exists(filename: String): Boolean = {
      var e: java.util.Enumeration[_] = libs.elements
      while (e.hasMoreElements) {
        var l: LibEntry = e.nextElement.asInstanceOf[LibEntry]
        if (filename.equals(l.lib)) return true
      }
      return false
    }

    def getLibs: Array[LibEntry] = {
      var l_array: Array[LibEntry] = new Array[LibEntry](libs.size)
      libs.copyInto(l_array.asInstanceOf[Array[AnyRef]])
      return l_array
    }

    def getColumnCount = 4

    def getRowCount = libs.size

    def getValueAt(row: Int, col: Int): AnyRef = {
      var obj: AnyRef = null
      if (row < libs.size) {
        var l: LibEntry = libs.elementAt(row)
        col match {
          case 0 => obj = l.lib
          case 1 => obj = l.doc
          case 2 => obj = new java.lang.Boolean(l.isOldJavaDoc)
          case 3 => obj = vis2string(l.visibility)
        }
      }
      obj
    }

    override def isCellEditable(row: Int, col: Int): Boolean = col > 0

    override def setValueAt(value: Any, row: Int, col: Int): Unit = {
      var l: LibEntry = libs.elementAt(row).asInstanceOf[LibEntry]
      col match {
        case 1 => {
          l.doc = value.toString
          if (l.doc.charAt(l.doc.length - 1) != '/') l.doc += '/'
        }
        case 2 => l.isOldJavaDoc = (value.asInstanceOf[Boolean]).booleanValue
        case 3 => {
          var old_visibility: Int = l.visibility
          l.visibility = string2vis(value.toString)
          if (old_visibility != l.visibility) libsChanged = true
        }
        case _ => ()
      }
      fireTableRowsUpdated(row, row)
    }

    override def getColumnName(index: Int): String =
      index match {
        case 0 => jEdit.getProperty("options.sindex.table.col0")
        case 1 => jEdit.getProperty("options.sindex.table.col1")
        case 2 => jEdit.getProperty("options.sindex.table.col2")
        case 3 => jEdit.getProperty("options.sindex.table.col3")
      }

    //TODO: In java you can cast as Boolean??
    override def getColumnClass(index: Int): Class[_] = if (index == 2) classOf[String] else classOf[String]

    def save: Unit = {
      var count: Int = 0

      var i: Int = 0
      while (i < libs.size) {
        {
          var l: LibEntry = libs.elementAt(i).asInstanceOf[LibEntry]
          if (l == null || l.lib == null || l.lib.length == 0
                  || l.doc == null || l.doc.length == 0) ()
          else {
            jEdit.setProperty("sindex.lib.name." + count, l.lib)
            jEdit.setProperty("sindex.lib.doc." + count, l.doc)
            jEdit.setProperty("sindex.lib.oldjdoc." + count, if (l.isOldJavaDoc) "true" else "false")
            jEdit.setProperty("sindex.lib.visibility." + count, vis2string(l.visibility))
            count += 1
          }
          i += 1
        }

      }
      jEdit.unsetProperty("sindex.lib.name." + count)
      jEdit.unsetProperty("sindex.lib.doc." + count)
      jEdit.unsetProperty("sindex.lib.oldjdoc." + count)
      jEdit.unsetProperty("sindex.lib.visibility." + count)
    }

    def entriesOk: Boolean = {
      if (libs.size == 0) {
        GUIUtilities.error(null, "options.sindex.error.nolibs", null)
        return false
      }
      var error = false

      {
        var i: Int = 0
        while (i < libs.size) {
          {
            var continue = false
            var l: LibEntry = libs.elementAt(i).asInstanceOf[LibEntry]
            if (l.lib == null || l.lib.length == 0) {
              GUIUtilities.error(null, "options.sindex.error.libmissing", Array[AnyRef](new Integer(i + 1)))
              error = true
              continue = true
            }
            else if (l.getLibFile == null) {
              GUIUtilities.error(null, "options.sindex.error.libwrong", Array[AnyRef](l.lib, new Integer(i + 1)))
              error = true
              continue = true
            }
            else if (l.doc == null || l.doc.length == 0) {
              GUIUtilities.error(null, "options.sindex.error.docmissing", Array[AnyRef](new Integer(i + 1)))
              error = true
              continue = true
            }
            if (!continue)
              try {
                var u: URL = new URL(l.doc)
              }
              catch {
                case e: MalformedURLException => {
                  GUIUtilities.error(null, "options.sindex.error.docwrong", Array[AnyRef](l.doc, new Integer(i + 1), e))
                  error = true
                }
              }
          }
          ({i += 1; i})
        }
      }
      !error
    }
  }

  private class ComboCellRenderer(model: Array[String]) extends JComboBox(model.asInstanceOf[Array[AnyRef]]) with TableCellRenderer {
    def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {
      setSelectedItem(value)
      return this
    }
  }
  init
}