// See LICENSE for license details.

package firrtl_interpreter.utils

import scala.annotation.tailrec
import scala.collection.mutable

object TSort {
  def apply(stringToStrings: Map[String, Set[String]], strings: Iterable[String]): Iterable[String] = {
    @tailrec
    def innerSort(toPreds: Map[String, Set[String]], done: Iterable[String]): Iterable[String] = {
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

    innerSort(stringToStrings, Seq())
  }

  def addMissingTerminals(stringToStrings: Map[String, Set[String]]): Map[String, Set[String]] = {
    val allSets: Iterable[Set[String]] = stringToStrings.values
    val allValues: Iterable[String] = allSets.flatten
    val distinctValues: List[String] = allValues.toList.distinct
    val newPairs: List[(String, Set[String])] = distinctValues.flatMap { value =>
      if (stringToStrings.contains(value)) {
        None
      }
      else {
        Some(value -> Set.empty[String])
      }
    }
    println(s"New Pairs: $newPairs")
    stringToStrings ++ newPairs.toMap
  }

  def findLoops(graph: Map[String, Set[String]]): Seq[Seq[String]] = {
    val loops = new mutable.HashSet[List[String]]

    def walk(children: Set[String], traversed: List[String]): Unit = {
      children.foreach { child: String =>
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