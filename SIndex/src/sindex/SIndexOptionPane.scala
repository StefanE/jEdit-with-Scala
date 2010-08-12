package sindex

import org.gjt.sp.jedit._
import javax.swing._
import javax.swing.border._
import java.awt.event._
import java.awt._

class SIndexOptionPane extends AbstractOptionPane("sindex") with ActionListener {
  private var bCreate: JButton = null
  private var cbFastDisplay: JCheckBox = null

  override def _save {
    jEdit.setProperty("sindex.fastDisplay", if (cbFastDisplay.isSelected) "true" else "false")
  }

  override def _init: Unit = {
    setBorder(new EmptyBorder(5, 5, 5, 5))
    var space: Dimension = new Dimension(0, 30)
    val cbFastDisplay = new JCheckBox(jEdit.getProperty("options.sindex.fastDisplay"))
    addComponent(cbFastDisplay)
    if ("false".equals(jEdit.getProperty("sindex.fastDisplay"))) cbFastDisplay.setSelected(false)
    else cbFastDisplay.setSelected(true)
    addComponent(new Box.Filler(space, space, space))
    bCreate = new JButton(jEdit.getProperty("options.sindex.createIndex"))
    bCreate.addActionListener(this)
    var cons: GridBagConstraints = new GridBagConstraints
    cons.gridy = {y += 1; y}
    cons.gridwidth = GridBagConstraints.REMAINDER
    cons.fill = GridBagConstraints.NONE
    cons.anchor = GridBagConstraints.CENTER
    gridBag.setConstraints(bCreate, cons)
    add(bCreate)
  }

  def actionPerformed(evt: ActionEvent) {new ConfigureDialog(jEdit.getFirstView)}
}