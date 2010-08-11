package sindex

import java.awt.event.{MouseEvent, MouseAdapter}
import org.gjt.sp.jedit.{jEdit, View}
import org.gjt.sp.util.Log
import java.awt._
import javax.swing._
import border.BevelBorder
import javax.swing.event.{DocumentListener, DocumentEvent, ListSelectionEvent, ListSelectionListener}
import sindex.SIndex.SearchResult
import sindex.SIndexDockable.TopicListModel

object SIndexDockable {
  val DOCKABLE_NAME: String = "plugin.SIndexPlugin"

  private val KEY_LIST_SIZE: Dimension = new Dimension(300, 400)
  private val TOPIC_LIST_SIZE: Dimension = new Dimension(300, 150)
  private val vertSpaceTop: Insets = new Insets(5, 5, 0, 5)
  private val vertSpaceNone: Insets = new Insets(0, 5, 0, 5)
  private val vertSpaceBoth: Insets = new Insets(5, 5, 5, 5)
  private val fixedCellHeight: Int = new JLabel("Mg_").getPreferredSize.height

  private val classIcon: ImageIcon = new ImageIcon(classOf[SIndexDockable].getResource("images/class.gif"))
  private val traitIcon: ImageIcon = new ImageIcon(classOf[SIndexDockable].getResource("images/trait.jpg"))
  private val constrIcon: ImageIcon = new ImageIcon(classOf[SIndexDockable].getResource("images/constr.gif"))
  private val methodIcon: ImageIcon = new ImageIcon(classOf[SIndexDockable].getResource("images/method.gif"))
  private val fieldIcon: ImageIcon = new ImageIcon(classOf[SIndexDockable].getResource("images/field.gif"))
  private val objectIcon: ImageIcon = new ImageIcon(classOf[SIndexDockable].getResource("images/object.jpg"))

  class TopicListModel extends AbstractListModel {
    def setEntries(entries: Array[IndexEntry]) {
      this.entries = entries
      fireContentsChanged(this, -1, -1)
    }

    def getElementAt(i: Int): AnyRef = {if (entries == null) "" else entries(i)}

    def getSize: Int = {if (entries == null) 0 else entries.length}

    private var entries: Array[IndexEntry] = null
  }

  class TopicListCellRenderer extends JLabel with ListCellRenderer {
    def getListCellRendererComponent(list: JList, value: Any, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = {
      var e: IndexEntry = value.asInstanceOf[IndexEntry]
      this.setText(e.sourceName)
      e.entryType match {
        case IndexEntry.CLAZZ => setIcon(SIndexDockable.classIcon)
        case IndexEntry.TRAIT => setIcon(SIndexDockable.traitIcon)
        case IndexEntry.OBJECT => setIcon(SIndexDockable.objectIcon)
        case IndexEntry.CONSTRUCTOR => setIcon(SIndexDockable.constrIcon)
        case IndexEntry.FIELD => setIcon(SIndexDockable.fieldIcon)
        case _ => setIcon(SIndexDockable.methodIcon)
      }
      if (isSelected) {
        this.setBackground(list.getSelectionBackground)
        this.setForeground(list.getSelectionForeground)
      }
      else {
        this.setBackground(list.getBackground)
        this.setForeground(list.getForeground)
      }
      this.setFont(list.getFont)
      this.setOpaque(true)
      this
    }
  }
}

class SIndexDockable(private var view: View) extends JPanel(new BorderLayout) with SIndexListener {
  private var index: SIndex = null
  private var searchField: JTextField = null
  private var keyList: HelpfulJList = null
  private var topicList: HelpfulJList = null
  private var tlmodel: TopicListModel = null
  private var status: JLabel = null
  private var splitpane: JSplitPane = null

  // search field
  searchField = new JTextField
  searchField.getDocument.addDocumentListener(new DocumentListener {
    def insertUpdate(e: DocumentEvent): Unit = {
      search
    }

    def changedUpdate(e: DocumentEvent): Unit = {
      search
    }

    private def search: Unit = {
      if (index == null) {
        setStatusText(prop("status.notloaded"))
        return
      }
      var searchstring: String = searchField.getText.trim
      setSearchResult(index.search(searchstring, false))
    }

    def removeUpdate(e: DocumentEvent): Unit = {
      search
    }
  })

  // label for search field
  var l1: JLabel = new JLabel(prop("search.label"))
  l1.setDisplayedMnemonic(prop("search.mnemonic").charAt(0))
  l1.setLabelFor(searchField)

  // list of keywords
  keyList = HelpfulJList(new KeyListModel)
  keyList.setPrototypeCellValue("W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_W_")
  keyList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  keyList.addListSelectionListener(new ListSelectionListener {
    def valueChanged(e: ListSelectionEvent): Unit = {
      if (index == null) return
      var keyPos: Int = keyList.getSelectedIndex
      if (keyPos < 0 || keyPos >= index.getNumKeywords) return
      updateTopicList(index.getEntriesAt(keyPos), keyPos)
    }
  })

  keyList.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      if (e.getClickCount == 2) if (topicList.getModel.getSize == 1) showDocOnSelectedEntry
      else topicList.requestFocus
    }
  })

  // label for list of keywords
  var l2: JLabel = new JLabel(prop("index.label"))
  l2.setDisplayedMnemonic(prop("index.mnemonic").charAt(0))
  l2.setLabelFor(keyList)
  // scrollpane for list of keywords
  var scrKeyList: JScrollPane = new JScrollPane(keyList)
  scrKeyList.setPreferredSize(SIndexDockable.KEY_LIST_SIZE)
  scrKeyList.setColumnHeaderView(l2)
  // list of topics found
  tlmodel = new TopicListModel
  topicList = HelpfulJList(tlmodel)
  topicList.setFixedCellHeight(SIndexDockable.fixedCellHeight)
  topicList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  topicList.setCellRenderer(new SIndexDockable.TopicListCellRenderer)
  topicList.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      if (e.getClickCount == 2) showDocOnSelectedEntry
    }
  })

  // label for list of topics
  var l3: JLabel = new JLabel(prop("topics.label"))
  l3.setDisplayedMnemonic(prop("topics.mnemonic").charAt(0))
  l3.setLabelFor(topicList)
  // scrollpane for list of topics
  var scrTopicList: JScrollPane = new JScrollPane(topicList)
  scrTopicList.setPreferredSize(SIndexDockable.TOPIC_LIST_SIZE)
  scrTopicList.setColumnHeaderView(l3)
  // splitpane with keylist and topiclist
  splitpane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, scrKeyList, scrTopicList)
  splitpane.setOneTouchExpandable(true)

  SwingUtilities.invokeLater(new Runnable {
    def run: Unit = {
      var divLoc: String = jEdit.getProperty("sindex.dividerLocation")
      if (divLoc != null) splitpane.setDividerLocation(Integer.parseInt(divLoc))
    }
  })

  // status label
  status = new JLabel(prop("status.label"))
  status.setBorder(new BevelBorder(BevelBorder.LOWERED))
  status.setFont(new Font("Dialog", Font.PLAIN, 10))
  // search field with label
  var top: Box = Box.createHorizontalBox
  top.add(l1)
  top.add(searchField)
  // general layout
  this.add(top, BorderLayout.NORTH)
  this.add(splitpane, BorderLayout.CENTER)
  this.add(status, BorderLayout.SOUTH)
  // set default size
  setSize(new Dimension(220, 500))
  // we like to be notified about SIndex and status changes:
  SIndexHolder.addSIndexListener(this)
  // initialize both lists:

  index = SIndexHolder.getIndex
  init

  private def setBusy(busy: Boolean) {
    var cursorType: Int = if (busy) Cursor.WAIT_CURSOR else Cursor.DEFAULT_CURSOR
    setCursor(Cursor.getPredefinedCursor(cursorType))
  }

  def indexChanged(e: SIndexChangeEvent): Unit = {
    var newIndex: SIndex = e.index
    var newStatus: Int = e.status
    Log.log(Log.DEBUG, this, "got index change, newStatus=" + newStatus)
    if (newStatus == SIndexHolder.STATUS_OK && newIndex != null) {
      index = newIndex
      init
      setBusy(false)
      setStatusText(prop("status.ready"))
    }
    else if (newStatus == SIndexHolder.STATUS_LOADING) {
      setBusy(true)
      setStatusText(prop("status.loading"))
    }
    else {
      setBusy(false)
      if (newStatus != SIndexHolder.STATUS_NOT_EXISTS && newStatus != SIndexHolder.STATUS_OK) setStatusText(prop("status.error"))
    }
  }

  override def getName: String = SIndexDockable.DOCKABLE_NAME

  def getComponent: Component = this

  override def removeNotify = {
    super.removeNotify
    SIndexHolder.removeSIndexListener(this)
    jEdit.setProperty("sindex.dividerLocation", Integer.toString(splitpane.getDividerLocation))
  }

  private def init {
    keyList.setModel(new KeyListModel)
    tlmodel.setEntries(null)
  }

  private def setStatusText(text: String): Unit = {
    SwingUtilities.invokeLater(new Runnable {
      def run {status.setText(text)}
    })
  }

  private def prop(key: String): String = jEdit.getProperty("sindex.frame." + key)

  def search(searchstring: String) {
    if (index == null) return
    if (searchstring == null) return
    var res: SearchResult = index.search(searchstring, true)
    if (res.entries == null) {
      setStatusText(jEdit.getProperty("sindex.frame.status.keywordnotfound", Array[AnyRef](res.searchstring)))
      Toolkit.getDefaultToolkit.beep
    }
    else {
      setSearchResult(res)
      if (res.entries.length == 1 && "true".equals(jEdit.getProperty("sindex.fastDisplay"))) {
        showDocOnEntry(res.entries(0))
      }
    }
  }

  private def setSearchResult(res: SearchResult): Unit = {
    val keyPos: Int = if (res.keywordpos >= 0) res.keywordpos else -res.keywordpos
    SwingUtilities.invokeLater(new Runnable {
      def run: Unit = {
        keyList.setSelectedIndex(keyPos)
        keyList.ensureIndexIsVisible(keyPos)
        updateTopicList(res.entries, keyPos)
      }
    })
  }

  private def updateTopicList(entries: Array[IndexEntry], keyPos: Int): Unit = {
    tlmodel.setEntries(entries)
    topicList.setSelectedIndex(0)
    setStatusText(jEdit.getProperty("sindex.frame.status.countEntries", Array[AnyRef](new Integer(if (entries == null) 0 else entries.length), index.getKeywordAt(keyPos))))
  }

  private def showDocOnSelectedEntry {
    var e: IndexEntry = topicList.getSelectedValue.asInstanceOf[IndexEntry]
    if (e != null) showDocOnEntry(e)
  }

  private def showDocOnEntry(e: IndexEntry) {
    setStatusText(jEdit.getProperty("sindex.frame.status.open", Array[AnyRef](e.sourceName)))
    var url: String = e.getCompleteURL
    Log.log(Log.DEBUG, this, "showDocOnEntry: " + url)
    openURL(url)
  }

  private def openURL(url: String) {infoviewer.InfoViewerPlugin.openURL(view, url)}
  
  private class KeyListModel extends AbstractListModel {
    def getElementAt(i: Int): AnyRef = {if (index == null) null else index.getKeywordAt(i)}

    def fireContentsChanged {fireContentsChanged(this, -1, -1)}

    def getSize = {if (index == null) 0 else index.getNumKeywords}
  }
}