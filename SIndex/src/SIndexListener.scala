package sindex

trait SIndexListener {
  def indexChanged(e: SIndexChangeEvent): Unit
}