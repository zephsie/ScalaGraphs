class Vertex[T](var data: T) {
  def getData: T = data

  def setData (data: T): Unit = {
    this.data = data
  }

  override def hashCode(): Int = {
    data.hashCode()
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Vertex[T] => this.data == that.data
      case _ => false
    }
  }
}

