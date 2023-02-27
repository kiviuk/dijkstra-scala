package shortestpaths

/** An undirected graph.
 *
 * The graph is constructed from its edge descriptions. The actual node and edge objects with full
 * navigation possibilities are then accessible via the `nodeMap`, `nodes` and `adjacencyMap`
 * properties.
 *
 * Nodes without any adjacent edges cannot be represented by this graph class (and they are not
 * relevant for this challenge).
 *
 * Example for graph construction:
 * {{{
 * val graph: UndirectedGraph = new UndirectedGraph(
 *       Seq(
 *         EdgeDesc(1, 2, 10),
 *         EdgeDesc(2, 3, 30),
 *       ),
 *     )
 * val node1: Node = graph.nodeMap(1)
 * val edgeToNode2: UndirectedEdge = node1.edgeTo(graph.nodeMap(2)).get
 * }}}
 */
class UndirectedGraph(private val edgeDescriptions: Seq[EdgeDesc]) {

  val nodeMap: Map[Int, Node] =
    edgeDescriptions.flatMap(e => Seq(e.nodeId1, e.nodeId2)).map(id => id -> Node(id, this)).toMap

  val nodes: Seq[Node] = nodeMap.values.toSeq

  val adjacencyMap: Map[Node, Seq[UndirectedEdge]] = edgeDescriptions
    .map(e => UndirectedEdge(nodeMap(e.nodeId1), nodeMap(e.nodeId2), e.length, this))
    .flatMap(e => Seq((e.node1, e), (e.node2, e)))
    .groupMap(pair => pair._1)(pair => pair._2)
}

/** Describes an undirected graph edge with a positive length. */
case class EdgeDesc(nodeId1: Int, nodeId2: Int, length: Float) {
  if (length <= 0) throw new IllegalArgumentException("Length must be strictly positive.")
}

/** A node in an undirected graph. */
case class Node(id: Int, graph: UndirectedGraph) {

  lazy val adjacentEdges: Seq[UndirectedEdge] = graph.adjacencyMap(this)

  /** Returns the edge between the current node and the given one (if existing). */
  def edgeTo(other: Node): Option[UndirectedEdge] =
    graph.adjacencyMap(this).find(_.endNodes == Set(this, other))

  /** Returns whether there is an edge between the current node and the given one. */
  def isAdjacent(other: Node): Boolean =
    graph.adjacencyMap(this).flatMap(_.endNodes).contains(other)

  override def toString: String = id.toString
}

/** An edge in an undirected graph. */
case class UndirectedEdge(node1: Node, node2: Node, length: Float, graph: UndirectedGraph) {
  if (length <= 0) throw new IllegalArgumentException("Length must be strictly positive.")

  val endNodes: Set[Node] = Set(node1, node2)

  /** Returns the other end of the edge, given one of the end nodes. */
  def otherEnd(start: Node): Node = endNodes.filter(_ != start).head

  /** Returns whether the current edge shares an end node with the given edge. */
  def isAdjacent(otherEdge: UndirectedEdge): Boolean =
    endNodes.intersect(otherEdge.endNodes).nonEmpty

  override def toString: String = "(" + node1.toString + "->" + node2.toString + ")"
}

/** An undirected path through a given graph.
 *
 * Example usage:
 * {{{
 * val graph: UndirectedGraph = new UndirectedGraph(
 *       Seq(
 *         EdgeDesc(1, 2, 10),
 *         EdgeDesc(2, 3, 30),
 *       ),
 *     )
 *
 * def node(id: Int) = graph.nodeMap(id)
 *
 * val path: UndirectedPath = UndirectedPath.from(node(1), node(2))
 * val samePath: UndirectedPath = new UndirectedPath(node(1), Seq(node(1).edgeTo(node(2)).get))
 *
 * val multiHopPath: UndirectedPath = path.append(path.end.edgeTo(node(3)).get)
 * println(multiHopPath.length) // Will print "40", the summed up length of edges 1<->2 and 2<->3
 * }}}
 */
case class UndirectedPath(start: Node, edges: Seq[UndirectedEdge]) {

  if (edges.isEmpty)
    throw new IllegalArgumentException("Path edges may not be empty")

  if (!edges.head.endNodes.contains(start))
    throw new IllegalArgumentException("Start node must be adjacent to first edge")

  if (!edges.drop(1).zip(edges.dropRight(1)).forall(pair => pair._1.isAdjacent(pair._2)))
    throw new IllegalArgumentException("Path edges must be a chain of adjacent nodes")

  val length: Float = edges.map(_.length).sum

  val nodes: Seq[Node] = {
    var nodes = Seq(start)
    for (edge <- edges)
      nodes = nodes ++ Seq(edge.otherEnd(nodes.last))
    nodes
  }

  val end: Node = nodes.last

  def prepend(edge: UndirectedEdge): UndirectedPath = {
    if (!edge.endNodes.contains(start))
      throw new IllegalArgumentException("Edge is not adjacent")
    UndirectedPath(edge.otherEnd(start), Seq(edge) ++ edges)
  }

  def append(edge: UndirectedEdge): UndirectedPath = {
    if (!edge.endNodes.contains(end))
      throw new IllegalArgumentException("Edge is not adjacent")
    UndirectedPath(start, edges ++ Seq(edge))
  }

  override def toString: String = "[" + nodes.map(_.id).mkString(", ") + "]"
}
object UndirectedPath {
  def from(nodes: Node*): UndirectedPath = {
    if (nodes.length < 2)
      throw new IllegalArgumentException("Paths with less than two nodes do not exist.")

    UndirectedPath(
      nodes.head,
      nodes.drop(1).zip(nodes.dropRight(1)).map(pair => pair._1.edgeTo(pair._2).get),
    )
  }
}
