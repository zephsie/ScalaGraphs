class Vertex[T](var data: T) {
  def getData: T = data

  def setData(data: T): Unit = {
    this.data = data
  }

  override def hashCode(): Int = {
    data.hashCode()
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null) return false
    if (!obj.isInstanceOf[Vertex[T]]) return false

    val other = obj.asInstanceOf[Vertex[T]]
    data.equals(other.data)
  }
}

