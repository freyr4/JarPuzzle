package yc.advcats

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Queue, Set => mSet}
import scala.util.control.Breaks.{break, breakable}

object JarPuzzle {

  case class Jar(cap: Int, contain: Int) {
    def add(amount: Int): Jar = Jar(cap, contain + amount)
  }

  case class Jars(js: List[Jar]) {
    def pour(j1: Int, j2: Int): Jars = {
      //j1 번째 jar 에서 j2 번째 jar 에 물을 부으면
      var jar1: Jar = js(j1)
      var jar2: Jar = js(j2)
      val jar2Remain = jar2.cap - jar2.contain

      if (jar2Remain == 0) ()
      else {
        if (jar1.contain >= jar2Remain) {
          jar1 = jar1.add(-jar2Remain)
          jar2 = jar2.add(jar2Remain)
        }
        else {
          jar2 = jar2.add(jar1.contain)
          jar1 = jar1.add(-jar1.contain)
        }
      }
      Jars(js.updated(j1, jar1).updated(j2, jar2))
    }
  }
  
  //깊이 우선 탐색
  def findStatus(first: Jars, target: Jars, rememberSet: mSet[Jars] = mSet(), path: ListBuffer[Jars] = ListBuffer()): ListBuffer[Jars] = {
    rememberSet.add(first)
    path.addOne(first)

    val len: Int = first.js.length
    breakable {
      for (i <- 0 until len; j <- 0 until len) {
        if (i == j) ()
        else {
          val nextJars = first.pour(i, j)

          if (!rememberSet(nextJars)) {
            if (target == nextJars) break
            else findStatus(nextJars, target, rememberSet, path)
          }
        }
      }
    }
    path
  }

  // 너비 우선 탐색
  def findStatusBFS(first: Jars, target: Jars): mutable.Map[Jars, Jars] = {
    val rememberSet: mSet[Jars] = mSet()
    val path: mutable.Map[Jars, Jars] = mutable.Map.empty
    val que: mutable.Queue[Jars] = mutable.Queue[Jars]()

    rememberSet.add(first)
    que.enqueue(first)

    val len: Int = first.js.length

    breakable {
      while (que.nonEmpty) {
        val nowNode = que.dequeue
        for (i <- 0 until len; j <- 0 until len) {
          if (i == j) ()
          else {
            val nextJars = nowNode.pour(i, j)

            if (!rememberSet(nextJars)) {
              rememberSet.add(nextJars)
              que.enqueue(nextJars)
              path.addOne((nowNode, nextJars))
              if (target == nextJars) break
            }
          }
        }
      }
    }
    path
  }

  def main(args: Array[String]): Unit = {
    val J10 = Jar(10, 10)
    val J7 = Jar(7, 0)
    val J3 = Jar(3, 0)

    val initialJars = Jars(List(J10, J7, J3))
    val problem1 = Jars(List(Jar(10, 5), Jar(7, 5), Jar(3, 0)))

    val start1 = System.currentTimeMillis()
    val sol = findStatus(initialJars, problem1)
    println(System.currentTimeMillis() - start1)

    sol.foreach(println)

    val start2 = System.currentTimeMillis()
    val sol2 = findStatusBFS(initialJars, problem1)
    println(System.currentTimeMillis() - start2)

    //sol2 경로를 복원하기 위해 따로 작업을.. 꼭 해야하나?
    val sol2reverse = for ((k, v) <- sol2) yield (v, k)
    var rev: Option[Jars] = Some(problem1)
    breakable {
      while (true) {
        val x: Option[Jars] = sol2reverse.get(rev.get)
        x match {
          case Some(j) =>
            println(j)
            rev = sol2reverse.get(rev.get)
          case None => break()
        }
      }
    }
  }
}
