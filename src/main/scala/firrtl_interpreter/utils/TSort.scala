// See LICENSE for license details.

package firrtl_interpreter.utils

import scala.annotation.tailrec
import scala.collection.mutable

object TSort {
  def apply[T](mapSet: Map[T, Set[T]], strings: Iterable[T]): Iterable[T] = {
    @tailrec
    def innerSort(toPreds: Map[T, Set[T]], done: Iterable[T]): Iterable[T] = {
      println(s"Partion: $toPreds")
      val (noPreds, hasPreds) = toPreds.partition {
        _._2.isEmpty
      }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) {
          done
        }
        else {
          sys.error(hasPreds.toString)
        }
      } else {
        val found = noPreds.keys
        innerSort(hasPreds.mapValues {
          _ -- found
        }, done ++ found)
      }
    }

    innerSort(mapSet, Seq())
  }

  def addMissingTerminals(mapSet: Map[String, Set[String]]): Map[String, Set[String]] = {
    val allSets: Iterable[Set[String]] = mapSet.values
    val allValues: Iterable[String] = allSets.flatten
    val distinctValues: List[String] = allValues.toList.distinct
    val newPairs: List[(String, Set[String])] = distinctValues.flatMap { value =>
      if (mapSet.contains(value)) {
        None
      }
      else {
        Some(value -> Set.empty[String])
      }
    }
    println(s"New Pairs: $newPairs")
    mapSet ++ newPairs.toMap
  }

  def showMissingTerminals[T](mapSet: Map[T, Set[T]]): Unit = {
    val allSets: Iterable[Set[T]] = mapSet.values
    val allValues: Iterable[T] = allSets.flatten
    val distinctValues: List[T] = allValues.toList.distinct
    val newPairs: List[(T, Set[T])] = distinctValues.flatMap { value =>
      if (mapSet.contains(value)) {
        None
      }
      else {
        Some(value -> Set.empty[T])
      }
    }
    println(s"Missing terminals: $newPairs")
  }

  def findLoops[T](graph: Map[T, Set[T]]): Seq[Seq[T]] = {
    val loops = new mutable.HashSet[List[T]]

    def walk(children: Set[T], traversed: List[T]): Unit = {
      children.foreach { child: T =>
        if (traversed.contains(child)) {
          loops += (child :: traversed).reverse.dropWhile(child.!=)
        }
        else if (graph.contains(child)) {
          walk(graph(child), child :: traversed)
        }
      }
    }

    walk(graph.keySet, Nil)
    loops.toSeq
  }
}