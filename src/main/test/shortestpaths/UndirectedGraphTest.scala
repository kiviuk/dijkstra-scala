package shortestpaths

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import shortestpaths.ShortestPaths.GraphPerturbation.{disruptPath, shortestPathsWithDisruption}

import scala.collection.immutable.Seq

class UndirectedGraphTest extends AnyFunSuite {

  val sampleGraphA: UndirectedGraph = new UndirectedGraph(
    Seq(
      EdgeDesc(1, 2, 10),
      EdgeDesc(1, 3, 15),
      EdgeDesc(2, 4, 12),
      EdgeDesc(2, 6, 15),
      EdgeDesc(4, 6, 1),
      EdgeDesc(4, 5, 2),
      EdgeDesc(3, 5, 10),
      EdgeDesc(6, 5, 5),
    ),
  )

  val sampleGraphB: UndirectedGraph = new UndirectedGraph(
    Seq(
      EdgeDesc(1, 2, 10),
      EdgeDesc(1, 3, 15),
      EdgeDesc(2, 4, 12),
      EdgeDesc(2, 6, 15),
    )
  )

  val sampleGraphC: UndirectedGraph = new UndirectedGraph(
    Seq(
      EdgeDesc(1, 2, 10),
      EdgeDesc(1, 3, 15),
      EdgeDesc(2, 4, 12),
      EdgeDesc(2, 6, 15),
      EdgeDesc(6, 5, 4),
      EdgeDesc(1, 5, 8),
      EdgeDesc(4, 5, 1),
    )
  )

  test("find expectedNearest unvisited node") {

    // given
    val nodeA = sampleGraphA.nodeMap(1)
    val nodeB = sampleGraphA.nodeMap(2)
    val nodeC = sampleGraphA.nodeMap(3)

    val unvisitedNodes = Set(nodeA, nodeB, nodeC)
    val distanceNodeMap: Map[Node, (Node, Double)] =
      Map(
        nodeA -> (nodeA, 100),
        nodeB -> (nodeA, 20),
        nodeC -> (nodeA, 10),
      )

    // when
    val expectedNearest: Node = ShortestPaths.findNearestUnvisitedNode(unvisitedNodes, distanceNodeMap)

    // then
    nodeC shouldBe expectedNearest
  }

  test("find nearest unvisited node for maxed out distance map") {

    // given
    val nodeA = sampleGraphA.nodeMap(1)
    val nodeB = sampleGraphA.nodeMap(2)
    val nodeC = sampleGraphA.nodeMap(3)

    val unvisitedNodes = Set(nodeA, nodeB, nodeC)
    val distanceNodeMap: Map[Node, (Node, Double)] =
      Map(
        nodeA -> (nodeA, Double.MaxValue),
        nodeB -> (nodeA, Double.MaxValue),
        nodeC -> (nodeA, Double.MaxValue),
      )

    // when
    val expectedNearest: Node = ShortestPaths.findNearestUnvisitedNode(unvisitedNodes, distanceNodeMap)

    // then
    nodeA shouldBe expectedNearest
  }

  test("find shortest path from node 5 to node 1") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(4, 6, 1),
        EdgeDesc(4, 5, 2),
        EdgeDesc(3, 5, 10),
        EdgeDesc(6, 5, 5),
      ),
    )
    val startNode: Node = graph.nodeMap(5)
    val distanceMap: Map[Node, (Node, Double)] =
      ShortestPaths.generateDijkstraDistanceMap(graph, startNode)

    // when
    val shortestPath: Seq[Node] = ShortestPaths.makePathSegments(startNode, graph.nodeMap(1), distanceMap)

    // then
    shortestPath should equal(List(
      graph.nodeMap(5), graph.nodeMap(4), graph.nodeMap(2), graph.nodeMap(1)))
  }

  test("find paths from node 5 to node 1 ranked by length") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(4, 6, 1),
        EdgeDesc(4, 5, 2),
        EdgeDesc(3, 5, 10),
        EdgeDesc(6, 5, 5),
      ),
    )
    val startNode: Node = graph.nodeMap(5)
    val endNode: Node = graph.nodeMap(1)

    val distanceMap: Map[Node, (Node, Double)] =
      ShortestPaths.generateDijkstraDistanceMap(graph, startNode)
    // when
    val shortestPath: Seq[Node] = ShortestPaths.makePathSegments(startNode, endNode, distanceMap)

    // close all edges to block the shortest path.
    val closedBestPath: Seq[Map[(Node, Node), Boolean]] =
      disruptPath(UndirectedPath.from(shortestPath: _*))(num => false)

    // repeat search with blocked best path
    val nextBestRoutes: Seq[UndirectedPath] =
      shortestPathsWithDisruption(graph, startNode, endNode, closedBestPath)

    // close the second best path and obstruct the best path
    val closedSecondBestPath: Seq[Map[(Node, Node), Boolean]] =
      disruptPath(nextBestRoutes.head) { _ => false }

    val disruptedBestPath: Seq[Map[(Node, Node), Boolean]] =
      disruptPath(UndirectedPath.from(shortestPath: _*)) { num => num % 2 != 0 }

    val combinedPaths: Seq[Map[(Node, Node), Boolean]] = disruptedBestPath.map { m =>
      closedSecondBestPath.foldLeft(m) { (acc, entry) => acc ++ entry }
    }

    val nextBestRoutes2: Seq[UndirectedPath] =
      shortestPathsWithDisruption(graph, startNode, endNode, combinedPaths)


    // then
    shortestPath should equal(List(
      graph.nodeMap(5), graph.nodeMap(4), graph.nodeMap(2), endNode))
  }

  test("maximise distance map 1") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
      )
    )

    val nodeA: Node = graph.nodeMap(1)
    val nodeB: Node = graph.nodeMap(2)
    val nodeC: Node = graph.nodeMap(3)
    nodeA

    // when
    val distanceMapA: Map[Node, (Node, Double)] = ShortestPaths.maximizeDijkstraDistanceMap(graph, nodeA)
    val distanceMapB: Map[Node, (Node, Double)] = ShortestPaths.maximizeDijkstraDistanceMap(graph, nodeB)
    val distanceMapC: Map[Node, (Node, Double)] = ShortestPaths.maximizeDijkstraDistanceMap(graph, nodeC)

    // then
    distanceMapA should equal(Map(
      nodeA -> (nodeA, 0f),
      nodeB -> (nodeB, Float.MaxValue),
      nodeC -> (nodeC, Float.MaxValue)))

    distanceMapB should equal(Map(
      nodeA -> (nodeA, Float.MaxValue),
      nodeB -> (nodeB, 0f),
      nodeC -> (nodeC, Float.MaxValue)))

    distanceMapC should equal(Map(
      nodeA -> (nodeA, Float.MaxValue),
      nodeB -> (nodeB, Float.MaxValue),
      nodeC -> (nodeC, 0f)))
  }

  test("find shortest path from node 6 to node 1") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(6, 5, 4),
        EdgeDesc(1, 5, 8),
        EdgeDesc(4, 5, 1),
      )
    )

    val startNode: Node = graph.nodeMap(6)
    val finalNode: Node = graph.nodeMap(1)

    // when
    val distanceMap: Map[Node, (Node, Double)] = ShortestPaths.generateDijkstraDistanceMap(graph, startNode)
    val shortestPath: Seq[Node] = ShortestPaths.makePathSegments(startNode, finalNode, distanceMap)

    // then
    shortestPath shouldBe List(
      graph.nodeMap(6), graph.nodeMap(5), graph.nodeMap(1)
    )
  }

  test("find shortest path from node 1 to node 2") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(6, 5, 4),
        EdgeDesc(1, 5, 8),
        EdgeDesc(4, 5, 1),
      )
    )

    val startNode: Node = graph.nodeMap(1)
    val finalNode: Node = graph.nodeMap(2)

    // when
    val distanceMap: Map[Node, (Node, Double)] = ShortestPaths.generateDijkstraDistanceMap(graph, startNode)
    val shortestPath: Seq[Node] = ShortestPaths.makePathSegments(startNode, finalNode, distanceMap)

    // then
    shortestPath shouldBe List(
      graph.nodeMap(1), graph.nodeMap(2)
    )
  }

  test("find shortest path from node 2 to node 1") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(6, 5, 4),
        EdgeDesc(1, 5, 8),
        EdgeDesc(4, 5, 1),
      )
    )

    // when
    val startNode: Node = graph.nodeMap(2)
    val finalNode: Node = graph.nodeMap(1)
    val distanceMap: Map[Node, (Node, Double)] = ShortestPaths.generateDijkstraDistanceMap(graph, startNode)

    val shortestPath: Seq[Node] = ShortestPaths.makePathSegments(startNode, finalNode, distanceMap)

    // then
    shortestPath shouldBe List(
      graph.nodeMap(2), graph.nodeMap(1)
    )
  }

  test("find shortest path from node 1 to node 1") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(6, 5, 4),
        EdgeDesc(1, 5, 8),
        EdgeDesc(4, 5, 1),
      )
    )

    // when
    val startNode: Node = graph.nodeMap(1)
    val finalNode: Node = graph.nodeMap(1)
    val distanceMap: Map[Node, (Node, Double)] = ShortestPaths.generateDijkstraDistanceMap(graph, startNode)

    val shortestPath: Seq[Node] = ShortestPaths.makePathSegments(startNode, finalNode, distanceMap)

    // then
    shortestPath shouldBe List(
      graph.nodeMap(1), graph.nodeMap(1)
    )
  }

  test("find minimum distance map for graph") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(6, 5, 4),
        EdgeDesc(1, 5, 8),
        EdgeDesc(4, 5, 1),
      )
    )

    // when
    val startNode: Node = graph.nodeMap(1)
    val distanceMap: Map[Node, (Node, Double)] = ShortestPaths.generateDijkstraDistanceMap(graph, startNode)

    // then
    distanceMap(startNode) shouldBe(startNode, 0d)
    distanceMap(graph.nodeMap(2)) shouldBe(graph.nodeMap(1), 10d)
    distanceMap(graph.nodeMap(3)) shouldBe(graph.nodeMap(1), 15d)
    distanceMap(graph.nodeMap(4)) shouldBe(graph.nodeMap(5), 9d)
    distanceMap(graph.nodeMap(5)) shouldBe(graph.nodeMap(1), 8d)
    distanceMap(graph.nodeMap(6)) shouldBe(graph.nodeMap(5), 12d)
  }

  test("find last node before destination") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
      )
    )
    val begin: Node = graph.nodeMap(1)
    val end: Node = graph.nodeMap(3)
    val expectedNode: Node = graph.nodeMap(2)

    // when happy
    val maybeFinalNodeBeforeDestination1: Option[Node] = ShortestPaths.findFinalNodeBeforeDestination(Seq(begin, expectedNode, end))
    val maybeFinalNodeBeforeDestination2: Option[Node] = ShortestPaths.findFinalNodeBeforeDestination(Seq(begin, end))

    // when sad
    val undefFinalNodeBeforeDestination1: Option[Node] = ShortestPaths.findFinalNodeBeforeDestination(Seq())
    val undefFinalNodeBeforeDestination2: Option[Node] = ShortestPaths.findFinalNodeBeforeDestination(Seq(begin))
    val undefFinalNodeBeforeDestination3: Option[Node] = ShortestPaths.findFinalNodeBeforeDestination(Seq(end))

    // then happy
    maybeFinalNodeBeforeDestination1 shouldBe Some(expectedNode)
    maybeFinalNodeBeforeDestination2 shouldBe Some(begin)

    // then sad
    undefFinalNodeBeforeDestination1 shouldBe None
    undefFinalNodeBeforeDestination2 shouldBe None
    undefFinalNodeBeforeDestination3 shouldBe None

  }

  test("new graph test") {

    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(4, 6, 1),
        EdgeDesc(4, 5, 2),
        EdgeDesc(3, 5, 10),
        EdgeDesc(6, 5, 5),
      )
    )

    val start: Node = graph.nodeMap(5)
    val end: Node = graph.nodeMap(1)

    val row1: Map[(Node, Node), Boolean] = Map(
      (graph.nodeMap(1), graph.nodeMap(2)) -> true,
      (graph.nodeMap(2), graph.nodeMap(4)) -> true,
      (graph.nodeMap(4), graph.nodeMap(5)) -> false,
      (graph.nodeMap(1), graph.nodeMap(3)) -> false,
      (graph.nodeMap(3), graph.nodeMap(5)) -> false,
    )

    val row2: Map[(Node, Node), Boolean] = Map(
      (graph.nodeMap(1), graph.nodeMap(2)) -> true,
      (graph.nodeMap(2), graph.nodeMap(4)) -> false,
      (graph.nodeMap(4), graph.nodeMap(5)) -> true,
      (graph.nodeMap(1), graph.nodeMap(3)) -> false,
      (graph.nodeMap(3), graph.nodeMap(5)) -> false,
    )

    val newGraph1: UndirectedGraph = ShortestPaths.newGraph(graph, row1)
    val newGraph2: UndirectedGraph = ShortestPaths.newGraph(graph, row2)

    val shortestPath1: UndirectedPath =
      ShortestPaths.shortestPaths(
        newGraph1,
        newGraph1.nodeMap(start.id),
        newGraph1.nodeMap(end.id)
      ).get

    val shortestPath2: UndirectedPath =
      ShortestPaths.shortestPaths(
        newGraph2,
        newGraph2.nodeMap(start.id),
        newGraph2.nodeMap(end.id)
      ).get

    import ShortestPaths.UndirectedPathComparator.isEqual
    isEqual(shortestPath1, UndirectedPath.from(start, graph.nodeMap(6), graph.nodeMap(4), graph.nodeMap(2), end))
    isEqual(shortestPath2, UndirectedPath.from(start, graph.nodeMap(4), graph.nodeMap(6), graph.nodeMap(2), end))

  }

  test("test adjacentEdges") {
    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(1, 3, 15),
        EdgeDesc(2, 4, 12),
        EdgeDesc(2, 6, 15),
        EdgeDesc(4, 6, 1),
        EdgeDesc(4, 5, 2),
        EdgeDesc(3, 5, 10),
        EdgeDesc(6, 5, 5),
      )
    )

    val currentNode: Node = graph.nodeMap(1)
    val nextNode: Node = graph.nodeMap(2)
    val undirectedEdge: UndirectedEdge = UndirectedEdge.apply(currentNode, nextNode, 15f, graph)

    // when
    val edge: UndirectedEdge = currentNode.adjacentEdges.find(e => e.isAdjacent(undirectedEdge)).get

    // then
    edge.node1 shouldBe undirectedEdge.node1
    edge.node2 shouldBe undirectedEdge.node2

  }

}

//    val shortestPath: UndirectedPath =
//      UndirectedPath.from(graph.nodeMap(1), graph.nodeMap(2), graph.nodeMap(4), graph.nodeMap(5))
//
//    val secondShortestPath: UndirectedPath =
//      UndirectedPath.from(graph.nodeMap(1), graph.nodeMap(3), graph.nodeMap(5))
//
//    val closedPath: Seq[Map[(Node, Node), Boolean]] = bindTraversabilityToPath(secondShortestPath) { _ => false }
//    val perturbedPath: Seq[Map[(Node, Node), Boolean]] = bindTraversabilityToPath(shortestPath) { num => num % 2 == 0 }
//
//    val combinedPaths: Seq[Map[(Node, Node), Boolean]] = perturbedPath.map { m =>
//      closedPath.foldLeft(m) { (acc, entry) =>
//        acc ++ entry
//      }
//    }
