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
    if (weight < 0) throw new IllegalArgumentException("Weight cannot be negative")

    if (weight == Int.MaxValue) throw new IllegalArgumentException("This value is reserved for infinity")

    val vertex1 = new Vertex[T](data1)
    val vertex2 = new Vertex[T](data2)

    if (graph.contains(vertex1) && graph.contains(vertex2)) graph(vertex1).put(vertex2, weight)
  }

  def removeEdge(data1: T, data2: T): Unit = {
    val vertex1 = new Vertex[T](data1)
    val vertex2 = new Vertex[T](data2)

    if (graph.contains(vertex1) && graph.contains(vertex2)) graph(vertex1).remove(vertex2)
  }

  def printGraph(): Unit = {
    graph.foreach { case (key, value) =>
      print(key.getData + " -> ")

      value.foreach { case (key, value) => print(key.getData + " (" + value + ") ") }

      println()
    }
  }

  private def getLowestCostNode(costs: mutable.HashMap[Vertex[T], Int], visited: mutable.HashSet[Vertex[T]]): Vertex[T] = {
    var lowestCost = Int.MaxValue
    var lowestCostNode: Vertex[T] = null

    costs.foreach { case (key, value) =>
      if (value < lowestCost && !visited.contains(key)) {
        lowestCost = value
        lowestCostNode = key
      }
    }

    lowestCostNode
  }

  def shortestPath(data1: T, data2: T): Seq[T] = {
    if (data1.equals(data2)) return Seq(data1)

    val vertex1 = new Vertex[T](data1)
    val vertex2 = new Vertex[T](data2)

    if (!graph.contains(vertex1) || !graph.contains(vertex2)) throw new IllegalArgumentException("Vertex does not exist")

    val costs = mutable.HashMap[Vertex[T], Int]()
    val parents = mutable.HashMap[Vertex[T], Vertex[T]]()
    val visited = mutable.HashSet[Vertex[T]]()

    graph.foreach { case (key, _) =>
      costs += (key -> (if (key == vertex1) 0 else Int.MaxValue))
      parents += (key -> null)
    }

    var node = getLowestCostNode(costs, visited)

    while (node != null) {
      val cost = costs(node)
      val neighbors = graph(node)

      neighbors.foreach { case (key, value) =>
        val newCost = cost + value

        if (costs(key) > newCost) {
          costs += (key -> newCost)
          parents += (key -> node)
        }
      }

      visited.add(node)
      node = getLowestCostNode(costs, visited)
    }

    val shortestPath = mutable.ListBuffer[T]()
    var parent = parents(vertex2)

    while (parent != null) {
      shortestPath.prepend(parent.getData)
      parent = parents(parent)
    }

    if (costs(vertex2) != Int.MaxValue) shortestPath.append(vertex2.getData)

    shortestPath.toSeq
  }

  private def getWeight(data1: T, data2: T): Int = {
    val vertex1 = new Vertex[T](data1)
    val vertex2 = new Vertex[T](data2)

    if (!graph.contains(vertex1) || !graph.contains(vertex2)) throw new IllegalArgumentException("Vertex does not exist")

    graph(vertex1)(vertex2)
  }

  def getPathLength(path: Seq[T]): Int = {
    if (path.isEmpty) throw new IllegalArgumentException("Path is empty")

    if (path.length == 1) return 0

    var pathLength = 0

    for (i <- 0 until path.length - 1) pathLength += getWeight(path(i), path(i + 1))

    pathLength
  }
}