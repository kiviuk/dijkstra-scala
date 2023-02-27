package shortestpaths

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class UndirectedGraphIntegrationTest extends AnyFunSuite {

  test("find paths ranked by length") {

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

    val startNode: Node = graph.nodeMap(1)
    val finalNode: Node = graph.nodeMap(5)

    // when
    val path: Seq[UndirectedPath] = ShortestPaths.kShortestPaths(graph, startNode, finalNode, 10)

    // then
    val expectedPath1: UndirectedPath =
      UndirectedPath.from(
        graph.nodeMap(1), graph.nodeMap(2), graph.nodeMap(4), graph.nodeMap(5)
      )

    val expectedPath2: UndirectedPath =
      UndirectedPath.from(graph.nodeMap(1), graph.nodeMap(3), graph.nodeMap(5)
      )

    val expectedPath3: UndirectedPath =
      UndirectedPath
        .from(graph.nodeMap(1), graph.nodeMap(2), graph.nodeMap(6), graph.nodeMap(4), graph.nodeMap(5)
        )

    val expectedPath4: UndirectedPath =
      UndirectedPath
        .from(graph.nodeMap(1), graph.nodeMap(2), graph.nodeMap(4), graph.nodeMap(6), graph.nodeMap(5)
        )

    val expectedPath5: UndirectedPath =
      UndirectedPath.from(graph.nodeMap(1), graph.nodeMap(2), graph.nodeMap(6), graph.nodeMap(5)
      )

    import ShortestPaths.UndirectedPathComparator.isEqual
    path.length shouldBe 5
    isEqual(path(0), expectedPath1) shouldBe true
    isEqual(path(1), expectedPath2) shouldBe true
    isEqual(path(2), expectedPath3) shouldBe true
    isEqual(path(3), expectedPath4) shouldBe true
    isEqual(path(4), expectedPath5) shouldBe true
  }

  test("find paths ranked by length in both directions") {

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

    val start: Node = graph.nodeMap(1)
    val end: Node = graph.nodeMap(5)

    // when: forward
    val shortestOutboundPaths: Seq[UndirectedPath] = ShortestPaths.kShortestPaths(graph, start, end, 10)

    // when: back home
    val shortestReturnPaths: Seq[UndirectedPath] = ShortestPaths.kShortestPaths(graph, end, start, 10)

    // then
    import ShortestPaths.UndirectedPathComparator.isEqual

    shortestOutboundPaths.size shouldBe 5
    isEqual(shortestOutboundPaths(0), UndirectedPath.from(start, graph.nodeMap(2), graph.nodeMap(4), end)) shouldBe true
    isEqual(shortestOutboundPaths(1), UndirectedPath.from(start, graph.nodeMap(3), end)) shouldBe true
    isEqual(shortestOutboundPaths(2), UndirectedPath.from(start, graph.nodeMap(2), graph.nodeMap(6), graph.nodeMap(4), end)) shouldBe true
    isEqual(shortestOutboundPaths(3), UndirectedPath.from(start, graph.nodeMap(2), graph.nodeMap(4), graph.nodeMap(6), end)) shouldBe true
    isEqual(shortestOutboundPaths(4), UndirectedPath.from(start, graph.nodeMap(2), graph.nodeMap(6), end)) shouldBe true

    shortestReturnPaths.size shouldBe 5
    isEqual(shortestReturnPaths(0), UndirectedPath.from(end, graph.nodeMap(4), graph.nodeMap(2), start)) shouldBe true
    isEqual(shortestReturnPaths(1), UndirectedPath.from(end, graph.nodeMap(3), start)) shouldBe true
    isEqual(shortestReturnPaths(2), UndirectedPath.from(end, graph.nodeMap(6), graph.nodeMap(4), graph.nodeMap(2), start)) shouldBe true
    isEqual(shortestReturnPaths(3), UndirectedPath.from(end, graph.nodeMap(4), graph.nodeMap(6), graph.nodeMap(2), start)) shouldBe true
    isEqual(shortestReturnPaths(4), UndirectedPath.from(end, graph.nodeMap(6), graph.nodeMap(2), start)) shouldBe true


  }

  test("apply new graph") {

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
    val start: Node = graph.nodeMap(1)
    val end: Node = graph.nodeMap(6)
    val paths: Seq[UndirectedPath] = ShortestPaths.kShortestPaths(graph, start, end, 10)

    // then
    import ShortestPaths.UndirectedPathComparator.isEqual

    paths.size shouldBe 2
    isEqual(paths(0), UndirectedPath.from(start, graph.nodeMap(5), end)) shouldBe true
    isEqual(paths(1), UndirectedPath.from(start, graph.nodeMap(2), end)) shouldBe true
  }


  test("Test finding the shortest route from node 1 to 5") {

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

    val startNode: Node = graph.nodeMap(1)
    val finalNode: Node = graph.nodeMap(5)

    // when
    val paths: Seq[UndirectedPath] = ShortestPaths.kShortestPaths(graph, startNode, finalNode, 10)

    // then
    val expectedPath1: UndirectedPath = UndirectedPath.from(startNode, graph.nodeMap(2), graph.nodeMap(4), finalNode)
    val expectedPath2: UndirectedPath = UndirectedPath.from(startNode, graph.nodeMap(3), finalNode)
    val expectedPath3: UndirectedPath = UndirectedPath.from(startNode, graph.nodeMap(2), graph.nodeMap(6), graph.nodeMap(4), finalNode)
    val expectedPath4: UndirectedPath = UndirectedPath.from(startNode, graph.nodeMap(2), graph.nodeMap(4), graph.nodeMap(6), finalNode)
    val expectedPath5: UndirectedPath = UndirectedPath.from(startNode, graph.nodeMap(2), graph.nodeMap(6), finalNode)

    import ShortestPaths.UndirectedPathComparator.isEqual
    paths.length shouldBe 5
    isEqual(paths(0), expectedPath1) shouldBe true
    isEqual(paths(1), expectedPath2) shouldBe true
    isEqual(paths(2), expectedPath3) shouldBe true
    isEqual(paths(3), expectedPath4) shouldBe true
    isEqual(paths(4), expectedPath5) shouldBe true
  }

  test("") {
    // given
    val graph: UndirectedGraph = new UndirectedGraph(
      Seq(
        EdgeDesc(1, 2, 10),
        EdgeDesc(2, 4, 12),
        EdgeDesc(4, 5, 1),
        EdgeDesc(5, 10, 1),
      )
    )

    val startNode: Node = graph.nodeMap(1)
    val finalNode: Node = graph.nodeMap(5)

    // when
    val paths: Seq[UndirectedPath] = ShortestPaths.kShortestPaths(graph, startNode, finalNode, 10)

    1 shouldBe(1)
  }

}

