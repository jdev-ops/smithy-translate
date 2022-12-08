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
package smithytranslate
package formatter

import smithytranslate.formatter.parsers.ShapeParser.shape_body
import cats.parse.Parser
import smithytranslate.formatter.writers.Writer
import smithytranslate.formatter.writers.Writer._
import smithytranslate.formatter.writers.ShapeWriter.shapeBodyWriter
import munit.Location

final class WriterSpec extends munit.FunSuite {
  private def parseAndFormat[B: Writer](
      e: Either[Parser.Error, (String, B)]
  )(implicit loc: Location): String = {
    assert(e.isRight, "Expected a right value")
    val (remains, value) =
      e.getOrElse(fail("Unable to extract a write value"))
    assert(remains.trim().isEmpty())
    value.write
  }

  test("write an array of mixins".only) {
    val formatted = parseAndFormat(
      shape_body.parse(
        """|structure ProfileUpdatedFull with [
           |    Mixin
           |    Mixin
           |    Mixin
           |    Mixin
           |    Mixin
           |    Mixin
           |    Mixin
           |] {}
           |""".stripMargin
      )
    )
    assertEquals(
      formatted,
      """|structure ProfileUpdatedFull with [
         |    Mixin,
         |    Mixin,
         |    Mixin,
         |    Mixin,
         |    Mixin,
         |    Mixin,
         |    Mixin,
         |] {
         |
         |}
         |""".stripMargin
    )
  }
}
