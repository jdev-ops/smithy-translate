/* Copyright 2022 Disney Streaming
 *
 * Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    https://disneystreaming.github.io/TOST-1.0.txt
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package smithyproto.proto3

import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.EnumShape
import software.amazon.smithy.model.shapes.ListShape
import software.amazon.smithy.model.shapes.MapShape
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.shapes.ResourceShape
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.shapes.ShapeVisitor
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.model.shapes.UnionShape

import scala.collection.mutable.{Set => MSet}
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.jdk.OptionConverters.RichOptional

object CircularDependencyCheck {

  private type FlatDependencies = Map[Namespace, Set[Namespace]]
  private type Namespace = String

  /** Inspect the provided model and returns true if it finds ciruclar
    * dependency between namespaces.
    *
    * The following example has such a dependency:
    *
    * file1.smithy:
    * ```
    * $version: "2"
    *
    * namespace ns.one
    *
    * structure Test1 {
    *   some: ns.two#Test2
    * }
    * ```
    *
    * and file2.smithy:
    * ```
    * $version: "2"
    *
    * namespace ns.two
    *
    * structure Test2 {
    *   some: ns.one#Test1
    * }
    * ```
    */
  def apply(m: Model): Option[List[String]] = {
    val visitor = new NamespaceDependencies(m)
    m.toSet().asScala.foreach { _.accept(visitor) }
    checkDeps(visitor.getRecordings())
  }

  /** Recursively check `allDependencies` to see whether there are circular
    * dependencies.
    *
    * There are two main components:
    *   - go1 - which process one namespace
    *   - go - which process all namespaces
    *
    * The logic is in go1, go is just a way to cover the entire model.
    *
    * `go1` works by exploring the model in a depth first approach. Let's assume
    * the flat dependency map looks like this:
    * ```
    * ns2 ->        [ns3]
    * ns3 ->        [ns4]
    * ns1 ->        [ns2]
    * ns5 ->        [ns0]
    * smithy.api -> []
    * ns0 ->        [ns1]
    * ns4 ->        [ns]
    * ```
    * The go1 starts with `None` for `current`, so in the first loop, it will
    * look at
    */
  private def checkDeps(
      allDependencies: FlatDependencies
  ): Option[List[Namespace]] = {
    // proccess one namespace, stop at the first conflict
    def go1(
        target: Namespace,
        current: Option[Namespace],
        rest: List[Namespace],
        result: List[Namespace]
    ): Option[List[Namespace]] = {
      current match {
        case Some(value) =>
          if (value == target) { // terminal case
            Some(value +: result)
          } else {
            val inDepth = allDependencies.getOrElse(value, Set.empty).toList
            inDepth match {
              case head :: next =>
                go1(
                  target,
                  Some(head),
                  next,
                  result :+ head
                )
              case Nil =>
                None
            }
          }
        case None =>
          rest match {
            case head :: _rest =>
              go1(target, Some(head), _rest, result :+ head)
            case Nil =>
              None
          }
      }
    }

    // process all namespaces, stop at the first conflict
    def go(
        _deps: List[(Namespace, Set[Namespace])],
        result: Option[List[Namespace]]
    ): Option[List[Namespace]] = {
      if (result.isDefined) {
        result
      } else {
        _deps match {
          case (ns, deps) :: next =>
            go(next, go1(ns, None, deps.toList, result = List.empty))
          case Nil =>
            None
        }
      }
    }
    go(allDependencies.toList, result = None)
  }

  private class NamespaceDependencies(m: Model)
      extends ShapeVisitor.Default[Unit] {

    private val _seen: MSet[String] = MSet.empty
    private val recordings: MSet[(Namespace, Set[Namespace])] = MSet.empty

    def getRecordings(): Map[Namespace, Set[Namespace]] = {
      recordings
        .groupMap(_._1)(_._2)
        .foldLeft(Map.empty[Namespace, Set[Namespace]]) {
          case (acc, (ns, deps)) =>
            val newDeps = acc.getOrElse(ns, Set.empty) ++ deps.toSet.flatten
            acc.updated(ns, newDeps)
        }
    }

    private def record(s: Shape, members: Set[ShapeId]) = {
      val key = s.getId().toString()
      if (_seen.add(key)) {
        val ns = s.getId.getNamespace
        val value =
          ns -> members.map(_.getNamespace()).toSet.filter(_ != ns)
        val _ = recordings.add(value)
        ()
      }
    }
    override protected def getDefault(
        shape: Shape
    ): Unit = record(shape, Set.empty)

    override def listShape(shape: ListShape): Unit = {
      record(shape, Set(shape.getMember().getTarget()))
    }

    override def mapShape(shape: MapShape): Unit = {
      record(shape, Set(shape.getKey().getTarget()))
      record(shape, Set(shape.getValue().getTarget()))
    }

    override def structureShape(
        shape: StructureShape
    ): Unit = {
      val targets = shape.getAllMembers().asScala.values.map(_.getTarget).toSet
      record(shape, targets)
    }

    override def unionShape(shape: UnionShape): Unit = {
      val targets = shape.getAllMembers().asScala.map(_._2.getTarget()).toSet
      record(shape, targets)
    }

    override def enumShape(shape: EnumShape): Unit = {
      val targets = shape.getAllMembers().asScala.map(_._2.getTarget()).toSet
      record(shape, targets)
    }

    override def operationShape(shape: OperationShape): Unit = {
      val targets = shape.getErrors().asScala.toSet +
        shape.getInputShape() +
        shape.getOutputShape()
      record(shape, targets)
    }

    override def serviceShape(shape: ServiceShape): Unit = {
      val targets = shape.getErrors().asScala.toSet ++
        shape.getAllOperations().asScala.toSet
      record(shape, targets)
    }
  }
}
