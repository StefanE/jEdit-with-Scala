package sindex

import org.gjt.sp.jedit.{EBPlugin, EBMessage, GUIUtilities, View}
import org.gjt.sp.jedit.gui.{OptionsDialog, DockableWindowManager}
import java.util.Vector
import javax.swing.JComponent

object SIndexPlugin {

  def lookup(view: View, token: String): Unit = {
    val mgr: DockableWindowManager = view.getDockableWindowManager
    if (mgr != null && mgr.isDockableWindowVisible(NAME)) {
      mgr.showDockableWindow(NAME)
      val win: JComponent = mgr.getDockable(NAME)
      val d: SIndexDockable = win.getComponent(0).asInstanceOf[SIndexDockable]
      d.search(token)
    }
    else {
      val dlg: MultipleEntriesDialog = new MultipleEntriesDialog(view, token)
      dlg.search(token)
    }
  }

  val NAME: String = SIndexDockable.DOCKABLE_NAME
}

class SIndexPlugin extends EBPlugin {
  @Deprecated
  override def handleMessage(message: EBMessage) {}

  @Deprecated
  override def start {}

  @Deprecated
  override def createOptionPanes(optionsDialog: OptionsDialog) {optionsDialog.addOptionPane(new SIndexOptionPane)}

  @Deprecated
  override def createMenuItems(menuItems: Vector[_]) {
    val menu = menuItems.asInstanceOf[Vector[AnyRef]]
    menu.addElement(GUIUtilities.loadMenu("sindex-menu"))
  }
}