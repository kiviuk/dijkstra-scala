package shortestpaths

import shortestpaths.Logger.INFO_IS_ENABLED
import shortestpaths.ShortestPaths.GraphPerturbation.{disruptPath, shortestPathsWithDisruption}
import shortestpaths.ShortestPaths.UndirectedPathComparator.removeDuplicatePaths

import scala.annotation.tailrec

object ShortestPaths extends App {

  /**
    * Computes and returns the K shortest paths between the given end nodes.
    *
    * The discovered paths always contain the shortest path between the two nodes. In addition, the
    * second shortest, third shortest and following paths are also added (in ascending order by path
    * length) up to (including) the k-th path.
    *
    * We do not constrain this function to acyclic paths, i.e., cyclic paths should be found as well.
    *
    * @param graph The undirected graph in which the N shortest paths shall be found.
    * @param start The start node of the paths
    * @param end   The end node of the paths
    * @param k     How many shortest paths to find (has to be greater than zero)
    * @return The discovered paths. If no shortest path from A to B exists, the result should be empty.
    */
  def kShortestPaths(
    graph: UndirectedGraph,
    start: Node,
    end: Node,
    k: Int
  ): Seq[UndirectedPath] = {

    // find the shortest path
    val bestPath: UndirectedPath = shortestPaths(graph, start, end).get

    // close all edges to block the shortest path
    val closedBestPath: Seq[Map[(Node, Node), Boolean]] = disruptPath(bestPath)(num => false)

    // repeat search
    val nextBestRoutes: Seq[UndirectedPath] = shortestPathsWithDisruption(graph, start, end, closedBestPath)

    // second best path (fixme: requires a recursive method to explore all paths that may exist within nextBestPaths??)
    val closedSecondBestPath: Seq[Map[(Node, Node), Boolean]] = if (nextBestRoutes.nonEmpty) {
      val secondBestPath: UndirectedPath = nextBestRoutes.head
      disruptPath(secondBestPath) { _ => false }
    } else {
      Seq()
    }

    // close all edges of the second best path and obstruct the best path
    val disruptedShortestPath: Seq[Map[(Node, Node), Boolean]] = disruptPath(bestPath) { num => num % 2 != 0 }

    val combinedPaths: Seq[Map[(Node, Node), Boolean]] = disruptedShortestPath.map { m =>
      closedSecondBestPath.foldLeft(m) { (acc, entry) =>
        acc ++ entry
      }
    }

    // fixme: at this point the search needs to repeat recursively (somehow) ....
    // fixme: but for now and the purpose of this toy demo we are done
    val nextBestRoutes2: Seq[UndirectedPath] = shortestPathsWithDisruption(graph, start, end, combinedPaths)

    removeDuplicatePaths(Seq(bestPath) ++ nextBestRoutes ++ nextBestRoutes2).take(k).sortBy(path => path.length)

  }

  def shortestPaths(
    graph: UndirectedGraph,
    startNode: Node,
    destinationNode: Node,
  ): Option[UndirectedPath] = {

    val dijkstraMap: Map[Node, (Node, Double)] = generateDijkstraDistanceMap(graph, startNode)

    val pathSegments: Seq[Node] = makePathSegments(startNode, destinationNode, dijkstraMap)

    if (INFO_IS_ENABLED) {
      println(s"DIJKSTRA: $dijkstraMap")
      println(s"SHORTEST: $pathSegments")
    }

    findFinalNodeBeforeDestination(pathSegments) match {
      case None => None
      case _ => Some(UndirectedPath.from(pathSegments: _*))
    }
  }

  def findFinalNodeBeforeDestination(pathSegments: Seq[Node]): Option[Node] = {
    pathSegments match {
      case init :+ _ if init.nonEmpty => Some(init.last)
      case _ => None
    }
  }

  /**
    * Computes the shortest paths from the start node to all other nodes in the graph using Dijkstra's method.
    *
    * @param graph the graph to compute the shortest paths on
    * @param start the starting node
    * @return a map that maps each node to a tuple of its predecessor node in the shortest path
    *         and the distance from the starting node
    */
  def generateDijkstraDistanceMap(graph: UndirectedGraph, start: Node): Map[Node, (Node, Double)] = {
    var visitedNodes: Set[Node] = Set.empty
    var unvisitedNodes: Set[Node] = Set(start)

    var dijkstraMap: Map[Node, (Node, Double)] = maximizeDijkstraDistanceMap(graph, start)

    while (unvisitedNodes.nonEmpty) {
      val currentNode: Node = findNearestUnvisitedNode(unvisitedNodes, dijkstraMap)

      unvisitedNodes = unvisitedNodes - currentNode

      currentNode
        .adjacentEdges
        .map(adjacentEdge => adjacentEdge.otherEnd(currentNode))
        .filter(adjacentNode => !visitedNodes.contains(adjacentNode))
        .foreach(unvisitedNode => {
          dijkstraMap = updateDijkstraDistanceMap(currentNode, unvisitedNode, dijkstraMap)
          unvisitedNodes = unvisitedNodes + unvisitedNode
        })

      visitedNodes = visitedNodes + currentNode
    }
    dijkstraMap
  }

  /**
    * Given an undirected graph and a starting node, initializes the distance map according to the Dijkstra's algo.
    *
    * @param graph the graph to create the distance map for
    * @param start the starting node to calculate distances from
    * @return a Map of nodes to their corresponding maximal possible distances from the starting node
    */
  def maximizeDijkstraDistanceMap(graph: UndirectedGraph, start: Node): Map[Node, (Node, Double)] = {
    val nodes: Seq[Node] = graph.nodes
    nodes.map(node =>
      node -> (node, if (start.id == node.id) {
        0d
      } else {
        Float.MaxValue
      })
    ).toMap
  }

  /**
    * Updates the distance map with the minimum distance from the source node to the visiting node.
    *
    * @param sourceNode   the node that leads to the visiting node in the current path
    * @param visitingNode the node to update the distance to
    * @param distanceMap  the current map of distances
    * @return a map that has been updated with the minimum distance from the source node to the visiting node
    */
  def updateDijkstraDistanceMap(
    sourceNode: Node,
    visitingNode: Node,
    distanceMap: Map[Node, (Node, Double)]): Map[Node, (Node, Double)] = {

    val currentEdgeLength: Double = UndirectedPath.from(sourceNode, visitingNode).length;

    val sourceNodeDistance: Double = distanceMap(sourceNode)._2
    val newDistanceToVisitingNode: Double = sourceNodeDistance + currentEdgeLength;
    val visitingNodeDistance: Double = distanceMap(visitingNode)._2;

    // Only update the distance of the visiting node if we haven't found a shorter path to it yet
    if (newDistanceToVisitingNode < visitingNodeDistance) {
      val newDistanceMapElement: (Node, Double) = (sourceNode, newDistanceToVisitingNode);
      distanceMap + (visitingNode -> newDistanceMapElement);
    } else {
      distanceMap
    }
  }

  /**
    * Returns a sequence of nodes that represents the shortest path between the start node
    * and the target node using the shortest distances provided in the distance map.
    *
    * @param startNode   the starting node of the path
    * @param targetNode  the target node that we want to reach
    * @param distanceMap a map that contains information about the distances between nodes.
    *                    The keys of the map are nodes, and the values are tuples that contain
    *                    the next node in the path and the shortest distance to the starting nodes.
    * @return a sequence of nodes representing the shortest path between the start and target nodes.
    */
  def makePathSegments(startNode: Node, targetNode: Node, distanceMap: Map[Node, (Node, Double)]): Seq[Node] = {
    buildPathHelper(startNode, targetNode, distanceMap, Seq(targetNode))
  }

  /**
    * A helper function that recursively builds the path from the start node to the current node
    * using the distances provided in the distance map.
    *
    * @param startNode   the starting node of the path
    * @param currentNode the current node being processed
    * @param distanceMap a map that contains information about the distances between nodes.
    *                    The keys of the map are nodes, and the values are tuples that contain
    *                    the next node in the path and the distance to the starting nodes.
    * @param pathSoFar   the sequence of nodes that have been visited so far on the path
    * @return a sequence of nodes representing the shortest path between the start and target nodes.
    */
  @tailrec
  def buildPathHelper(
    startNode: Node,
    currentNode: Node,
    distanceMap: Map[Node, (Node, Double)],
    pathSoFar: Seq[Node]): Seq[Node] = {

    val predecessorNode: Node = distanceMap.get(currentNode).head._1

    // loop detected: exit to escape infinity
    lazy val exitIfLoopDetected: Boolean = currentNode.id == predecessorNode.id
    lazy val reachableEdgeDetected: Boolean = !(Math.abs(distanceMap.get(currentNode).head._2 - Double.MaxValue) < 1e-6)
    lazy val edgeIsNotProhibited: Boolean = startNode.edgeTo(currentNode).get.length < Float.MaxValue

    if (startNode.id == currentNode.id) {
      startNode +: pathSoFar
    } else if (startNode.isAdjacent(currentNode)
      && startNode.id == predecessorNode.id
      && edgeIsNotProhibited) {
      startNode +: pathSoFar
    } else if (exitIfLoopDetected) {
      pathSoFar
    } else {
      val newSeq: Seq[Node] = if (reachableEdgeDetected) {
        pathSoFar.+:(predecessorNode)
      } else {
        pathSoFar.+:(predecessorNode).filter(n => n.id != currentNode.id)
      }
      buildPathHelper(startNode, predecessorNode, distanceMap, newSeq)
    }
  }

  /**
    * Finds the unvisited node with the shortest distance in line with the distance map.
    *
    * @param unvisitedNodes      a set of unvisited nodes
    * @param dijkstraDistanceMap a map of distances from the starting node to each node
    *                            in the graph
    * @return the unvisited node with the shortest distance from the starting node
    */
  def findNearestUnvisitedNode(unvisitedNodes: Set[Node], dijkstraDistanceMap: Map[Node, (Node, Double)]): Node = {

    var nearestNode: Node = unvisitedNodes.head
    var shortestLength: Double = Double.MaxValue

    // Loop through the unvisited set of nodes and find the one with
    // the shortest distance to the starting node
    // Although PriorityQueues will be a more efficient choice, I opted out due the existing inefficiency of my implementation
    unvisitedNodes.foreach(node => {
      val length: Double = dijkstraDistanceMap(node)._2
      if (length < shortestLength) {
        shortestLength = length
        nearestNode = node
      }
    })
    nearestNode
  }

  /**
    * Given an undirected graph and a binary map, returns a new graph with edges marked as traversable or not based on
    * the binary map. Edges not found in the binary map are considered traversable by default.
    *
    * @param originalGraph                the original undirected graph to be modified
    * @param disruptedAndTraversableEdges a map containing pairs of nodes and a boolean value indicating if the corresponding edge is
    *                                     traversable or not
    * @return a new undirected graph with edges marked as traversable or not based on the binary map
    */
  def newGraph(originalGraph: UndirectedGraph, disruptedAndTraversableEdges: Map[(Node, Node), Boolean]): UndirectedGraph = {

    def isEdgeTraversable(edge: UndirectedEdge): Boolean = {
      disruptedAndTraversableEdges.find {
        case ((node1, node2), _) => node1.id == edge.node1.id && node2.id == edge.node2.id
        case ((node2, node1), _) => node1.id == edge.node1.id && node2.id == edge.node2.id
      } match {
        case Some((_, value)) => value
        case None => true
      }
    }

    new UndirectedGraph(
      originalGraph.adjacencyMap.flatMap {
        case (_, edges) =>
          edges.distinct.map {
            case edge if isEdgeTraversable(edge) =>
              EdgeDesc(edge.node1.id, edge.node2.id, edge.length)
            case edge =>
              EdgeDesc(edge.node1.id, edge.node2.id, Float.MaxValue)
          }
      }.toSeq.distinct)
  }

  object GraphPerturbation {

    /**
      * Generates a sequence of maps representing the possible traversability of each edge in a given path.
      * The traversability of each edge is determined by a provided boolean function that takes an integer input and returns true or false.
      *
      * @param path a sequence of pairs of nodes representing the edges in the path.
      * @param f    a boolean function that takes an integer input and returns true or false, used to determine the traversability of each edge.
      * @return a sequence of maps, where each map represents a possible combination of traversable edges in the path.
      *         Each map is a key-value pair, where the key is an edge represented as a pair of nodes, and the value is a boolean
      *         indicating whether the edge is traversable or not.
      *
      */
    def bindTraversabilityToEdges(path: Seq[(Node, Node)])(f: Int => Boolean): Seq[Map[(Node, Node), Boolean]] = {

      def createBinaryMatrix(numberPathSegments: Int): Array[Seq[Boolean]] = {
        val numRowsInMatrix: Int = math.pow(2, numberPathSegments).toInt
        val matrix: Array[Array[Boolean]] = Array.ofDim[Boolean](numRowsInMatrix, numberPathSegments)

        // Populate matrix with all possible binary states from 0 to (2^N)-1: (0,0,0,...), ..., (1,1,1,...)
        for (row <- 0 until numRowsInMatrix) {
          var num: Int = row
          for (col <- numberPathSegments - 1 to 0 by -1) {
            matrix(row)(col) = f(num)
            num = num / 2
          }
        }
        if (Logger.DEBUG_IS_ENABLED) {
          matrix.foreach(x => println(x.mkString(" ")))
        }
        matrix.map(_.toSeq)
      }

      val numberPathSegments: Int = path.length
      val binaryMatrix: Seq[Seq[Boolean]] = createBinaryMatrix(numberPathSegments)

      val edgesWithTraversability: Seq[Map[(Node, Node), Boolean]] = binaryMatrix
        .map(row => row.zip(path)
          .flatMap {
            case (onOrOff, edge) => Map(edge -> onOrOff)
          }.toMap
        )

      if (Logger.DEBUG_IS_ENABLED) {
        for (i <- edgesWithTraversability.indices) {
          println(s"Map ${i + 1}:")
          for ((edge, isTraversable) <- edgesWithTraversability(i)) {
            println(s"$edge -> " + (if (isTraversable) {
              "1"
            } else {
              "0"
            }))
          }
          println()
        }
      }

      edgesWithTraversability
    }

    def disruptPath(path: UndirectedPath)(f: Int => Boolean): Seq[Map[(Node, Node), Boolean]] = {
      bindTraversabilityToEdges(nodesFromUndirectedPath(path))(f)
    }

    def nodesFromUndirectedPath(path: UndirectedPath): Seq[(Node, Node)] = {
      path.edges.map(edge => (edge.node1, edge.node2))
    }

    def shortestPathsWithDisruption(
      graph: UndirectedGraph,
      start: Node,
      end: Node,
      paths: Seq[Map[(Node, Node), Boolean]]): Seq[UndirectedPath] = {
      removeDuplicatePaths(
        paths
          .map(row => ShortestPaths.newGraph(graph, row))
          .map(newGraph => ShortestPaths.shortestPaths(newGraph, newGraph.nodeMap(start.id), newGraph.nodeMap(end.id)))
          .filter(path => path.nonEmpty)
          .map(path => path.get)
      ).sortBy(path => path.length)
    }
  }

  object UndirectedPathComparator {

    private[UndirectedPathComparator] implicit class UndirectedPathComparator(thisPath: UndirectedPath) {

      def nodesAreEqualInOrder(otherPath: UndirectedPath): Boolean = {
        def nodeSeqsAreEqual(seq1: Seq[Node], seq2: Seq[Node]): Boolean = {
          if (seq1.size != seq2.size) {
            false
          } else {
            seq1.map(_.id) == seq2.map(_.id)
          }
        }

        nodeSeqsAreEqual(thisPath.nodes, otherPath.nodes)
      }
    }

    def isEqual(pathA: UndirectedPath, pathB: UndirectedPath): Boolean = {
      pathA.nodesAreEqualInOrder(pathB)
    }

    def removeDuplicatePaths(seq: Seq[UndirectedPath]): Seq[UndirectedPath] = {
      seq.foldLeft(Seq.empty[UndirectedPath]) { (allPaths, path) =>
        if (allPaths.exists(_.nodesAreEqualInOrder(path))) {
          allPaths
        } else {
          allPaths :+ path
        }
      }
    }
  }

}

object Logger {

  val DEBUG_IS_ENABLED: Boolean = false
  val INFO_IS_ENABLED: Boolean = false

}
