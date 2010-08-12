package sindex

import org.gjt.sp.jedit.gui.EnhancedDialog
import org.gjt.sp.util.Log
import sindex.SIndex.SearchResult
import org.gjt.sp.jedit.{GUIUtilities, View, jEdit}
import sindex.SIndexDockable.TopicListModel
import java.awt._
import event.{MouseEvent, MouseAdapter}
import javax.swing._
import javax.swing.border.BevelBorder

object MultipleEntriesDialog {
  private val TOPIC_LIST_SIZE: Dimension = new Dimension(300, 150)
  private val vertSpaceTop: Insets = new Insets(5, 5, 0, 5)
  private val vertSpaceNone: Insets = new Insets(0, 5, 0, 5)
  private val vertSpaceBoth: Insets = new Insets(5, 5, 5, 5)
  private val fixedCellHeight: Int = new JLabel("Mg_").getPreferredSize.height
}

class MultipleEntriesDialog(
        private var view: View,
        private var searchString: String) extends
EnhancedDialog(view, jEdit.getProperty("sindex.chooser.title", Array[AnyRef](searchString)), false) with SIndexListener {
  private var index: SIndex = null
  private var lastSearchString: String = null
  private var topicList: HelpfulJList = null
  private var tlmodel: TopicListModel = null
  private var status: JLabel = null

  def init() {
    // info label
    val small = new Font("Dialog", Font.PLAIN, 10)
    val info = new JLabel(jEdit.getProperty("sindex.chooser.info"))
    info.setFont(small)

    // list of topics found
    tlmodel = new SIndexDockable.TopicListModel()
    topicList = HelpfulJList(tlmodel)
    topicList.setFixedCellHeight(MultipleEntriesDialog.fixedCellHeight)
    topicList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    topicList.setCellRenderer(new SIndexDockable.TopicListCellRenderer())
    topicList.addMouseListener(new MouseAdapter() {
      override def mouseClicked(e: MouseEvent) {
        if (e.getClickCount() == 2)
          ok()
      }
    })

    // scrollpane for list of topics
    val scrTopicList = new JScrollPane(topicList)
    scrTopicList.setPreferredSize(MultipleEntriesDialog.TOPIC_LIST_SIZE)

    // status label
    status = new JLabel(jEdit.getProperty("sindex.frame.status.label"))
    status.setFont(small)
    status.setBorder(new BevelBorder(BevelBorder.LOWERED))

    // general layout
    getContentPane().setLayout(new BorderLayout())
    getContentPane().add(scrTopicList, BorderLayout.CENTER)
    getContentPane().add(info, BorderLayout.NORTH)
    getContentPane().add(status, BorderLayout.SOUTH)
    setSize(new Dimension(300, 150))
    setLocationRelativeTo(view)
    GUIUtilities.loadGeometry(this, "sindex.chooser")

    // misc setup
    SIndexHolder.addSIndexListener(this);
    index = SIndexHolder.getIndex // note: the index may still be null

    // if index is not yet loaded, make window visible to see progress:
    if (index == null)
      setVisible(true)
  }

  def indexChanged(e: SIndexChangeEvent): Unit = {
    var newIndex: SIndex = e.index
    var newStatus: Int = e.status
    if ((newStatus == SIndexHolder.STATUS_OK) && newIndex != null) {
      index = newIndex
      setBusy(false)
      setStatusText(jEdit.getProperty("sindex.frame.status.ready"))
      tlmodel.setEntries(null)
      if (lastSearchString != null) search(lastSearchString)
    }
    else if (newStatus == SIndexHolder.STATUS_LOADING) {
      setBusy(true)
      setStatusText(jEdit.getProperty("sindex.frame.status.loading"))
    }
    else {
      setBusy(false)
      if (newStatus != SIndexHolder.STATUS_NOT_EXISTS && newStatus != SIndexHolder.STATUS_OK) setStatusText(jEdit.getProperty("sindex.frame.status.error"))
    }
  }

  private def setStatusText(text: String): Unit = {
    SwingUtilities.invokeLater(new Runnable {
      def run {status.setText(text)}
    })
  }

  private def setBusy(busy: Boolean) {
    val cursorType: Int = if (busy) Cursor.WAIT_CURSOR else Cursor.DEFAULT_CURSOR
    setCursor(Cursor.getPredefinedCursor(cursorType))
  }

  def ok {
    showDocOnSelectedEntry
    setVisible(false)
    SIndexHolder.removeSIndexListener(new SIndexListener {def indexChanged(e: SIndexChangeEvent) = {}})
    index = null
    dispose
  }

  def cancel {
    setVisible(false)
    SIndexHolder.removeSIndexListener(new SIndexListener {def indexChanged(e: SIndexChangeEvent) = {}})
    index = null
    dispose
  }

  override def setVisible(vis: Boolean) {
    super.setVisible(vis)
    if (!vis) GUIUtilities.saveGeometry(this, "sindex.chooser")
  }

  def search(searchstring: String): Unit = {
    if (index == null) {
      setVisible(true)
      setStatusText(jEdit.getProperty("sindex.frame.status.notloaded"))
      lastSearchString = searchstring
      return
    }
    if (searchstring == null) return
    SwingUtilities.invokeLater(new Runnable {
      def run: Unit = {
        setTitle(jEdit.getProperty("sindex.chooser.title", Array[AnyRef](searchstring)))
      }
    })
    var res: SearchResult = index.search(searchstring, true)
    if (res.entries == null) {
      setStatusText(jEdit.getProperty("sindex.frame.status.keywordnotfound", Array[AnyRef](res.searchstring)))
      Toolkit.getDefaultToolkit.beep
    }
    else {
      setSearchResult(res.entries, index.getKeywordAt(res.keywordpos))
      if (res.entries.length == 1 && "true".equals(jEdit.getProperty("sindex.fastDisplay"))) {
        showDocOnEntry(res.entries(0))
        setVisible(false)
      }
      else {
        topicList.requestFocus
        setVisible(true)

      }
    }
    lastSearchString = null
  }


  private def setSearchResult(entries: Array[IndexEntry], keyword: String): Unit = {
    tlmodel.setEntries(entries)
    SwingUtilities.invokeLater(new Runnable {
      def run: Unit = {
        topicList.setSelectedIndex(0)
      }
    })
    val value = if (entries == null) 0.asInstanceOf[AnyRef] else entries.length.asInstanceOf[AnyRef]
    setStatusText(jEdit.getProperty("sindex.frame.status.countEntries", Array[AnyRef](value, keyword)))
  }

  private def showDocOnSelectedEntry {
    val e: IndexEntry = topicList.getSelectedValue.asInstanceOf[IndexEntry]
    if (e != null) showDocOnEntry(e)
  }

  private def showDocOnEntry(e: IndexEntry) {
    setStatusText(jEdit.getProperty("sindex.frame.status.open", Array[AnyRef](e.docName)))
    var url: String = e.getCompleteURL
    Log.log(Log.DEBUG, this, "showDocOnEntry: " + url)
    openURL(url)
  }

  private def openURL(url: String) {infoviewer.InfoViewerPlugin.openURL(view, url)}
  init()
}