package sindex

import java.awt._
import java.awt.event._
import javax.swing._

object HelpfulJList {
  def apply() = {new JList() with HelpfulJList}

  def apply(model: ListModel) = {new JList(model) with HelpfulJList}
}

trait HelpfulJList extends JList with MouseListener {
  ToolTipManager.sharedInstance.registerComponent(this)
  addMouseListener(this)

  final override def getToolTipText(evt: MouseEvent): String = {
    var index: Int = locationToIndex(evt.getPoint)
    if (index >= 0) {
      var item: Any = getModel.getElementAt(index)
      var renderer: Component = getCellRenderer.getListCellRendererComponent(this, item, index, isSelectedIndex(index), false)
      var cellSize: Dimension = renderer.getPreferredSize
      var cellBounds: Rectangle = getCellBounds(index, index)
      if (cellBounds != null) {
        var cellRect: Rectangle = new Rectangle(0, cellBounds.y, cellSize.width, cellBounds.height)
        if (!cellRectIsVisible(cellRect)) return item.toString.asInstanceOf[String]
      }
    }
    return null
  }

  def mouseEntered(evt: MouseEvent) {
    var ttm: ToolTipManager = ToolTipManager.sharedInstance
    toolTipInitialDelay = ttm.getInitialDelay
    ttm.setInitialDelay(0)
  }

  def mouseReleased(evt: MouseEvent) {}

  def mouseClicked(evt: MouseEvent) {}

  def mousePressed(evt: MouseEvent) {}

  def mouseExited(evt: MouseEvent) {
    var ttm: ToolTipManager = ToolTipManager.sharedInstance
    if (toolTipInitialDelay >= 0) ttm.setInitialDelay(toolTipInitialDelay)
  }

  private var toolTipInitialDelay: Int = -1

  final override def getToolTipLocation(evt: MouseEvent): Point = {
    var index: Int = locationToIndex(evt.getPoint)
    if (index >= 0) {
      var item: Any = getModel.getElementAt(index)
      var renderer: Component = getCellRenderer.getListCellRendererComponent(this, item, index, isSelectedIndex(index), false)
      var cellSize: Dimension = renderer.getPreferredSize
      var cellBounds: Rectangle = getCellBounds(index, index)
      if (cellBounds != null) {
        var cellRect: Rectangle = new Rectangle(cellBounds.x, cellBounds.y, cellSize.width, cellBounds.height)
        if (!cellRectIsVisible(cellRect)) {
          var offs: Int = 0
          if (renderer.isInstanceOf[JLabel]) {
            var icon: Icon = (renderer.asInstanceOf[JLabel]).getIcon
            if (icon != null) offs += icon.getIconWidth
          }
          return new Point(cellRect.x + offs, cellRect.y)
        }
      }
    }
    return null
  }

  private def cellRectIsVisible(cellRect: Rectangle): Boolean = {
    var vr: Rectangle = getVisibleRect
    return vr.contains(cellRect.x + 22, cellRect.y + 2) && vr.contains(cellRect.x + 22 + cellRect.width - 31, cellRect.y + 2 + cellRect.height - 4)
  }
}