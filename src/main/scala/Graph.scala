import scala.collection.mutable

class Graph[T]() {
  private val graph = mutable.HashMap[Vertex[T], mutable.HashMap[Vertex[T], Int]]()

  def addVertex(data: T): Unit = {
    graph.put(new Vertex[T](data), mutable.HashMap[Vertex[T], Int]())
  }

  def removeVertex(data: T): Unit = {
    val vertex = new Vertex[T](data)
    graph.remove(vertex)
    graph.foreach { case (_, value) => value.remove(vertex) }
  }

  def addEdge(data1: T, data2: T, weight: Int): Unit = {
    val vertex1 = new Vertex[T](data1)
    val vertex2 = new Vertex[T](data2)

    if (graph.contains(vertex1) && graph.contains(vertex2)) {
      graph(vertex1).put(vertex2, weight)
    }
  }

  def removeEdge(data1: T, data2: T): Unit = {
    val vertex1 = new Vertex[T](data1)
    val vertex2 = new Vertex[T](data2)

    if (graph.contains(vertex1) && graph.contains(vertex2)) {
      graph(vertex1).remove(vertex2)
    }
  }

  def printGraph(): Unit = {
    graph.foreach { case (key, value) =>
      print(key.getData + " -> ")
      value.foreach { case (key, value) =>
        print(key.getData + " (" + value + "), ")
      }
      println()
    }
  }
}