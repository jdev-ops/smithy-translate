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

import munit._
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.ListShape
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.StructureShape

class CircularDependencyCheckSpec extends FunSuite {
  test("N level deep, no circular") {
    def forDepth(n: Int): Model = {
      val mb = Model.assembler()
      0.to(n).foreach { i =>
        val last = i == n
        val ns = s"ns$i"
        val targetNs = if (last) "ns0" else s"ns${i + 1}"
        mb.addShape(
          StructureShape
            .builder()
            .id(s"$ns#SomeStruct")
            .addMember(
              MemberShape
                .builder()
                .id(s"$ns#SomeStruct$$value")
                .target(
                  if (last) "smithy.api#String" else s"$targetNs#SomeStruct"
                )
                .build()
            )
            .build()
        )
      }
      mb.assemble().unwrap()
    }

    Assertions.assertEquals(CircularDependencyCheck(forDepth(5)), None)
  }

  test("circular N level deep") {
    def forDepth(n: Int): Model = {
      val mb = Model.assembler()
      0.to(n).foreach { i =>
        val last = i == n
        val ns = s"ns$i"
        val targetNs = if (last) "ns0" else s"ns${i + 1}"
        mb.addShape(
          StructureShape
            .builder()
            .id(s"$ns#SomeStruct")
            .addMember(
              MemberShape
                .builder()
                .id(s"$ns#SomeStruct$$value")
                .target(s"$targetNs#SomeStruct")
                .build()
            )
            .build()
        )
      }
      mb.assemble().unwrap()
    }

    val n = 5
    val expected = Some(
      "ns2" :: "ns3" :: "ns4" :: "ns5" :: "ns0" :: "ns1" :: "ns2" :: Nil
    )
    Assertions.assertEquals(CircularDependencyCheck(forDepth(n)), expected)
  }

  test("compile a simple smithy model") {
    val model = {
      val mb = Model.assembler()
      mb.addShape(
        StructureShape
          .builder()
          .id(s"random#SomeStruct")
          .addMember(
            MemberShape
              .builder()
              .id("random#SomeStruct$myList")
              .target("other#MyList")
              .build()
          )
          .build()
      )
      mb.addShape(
        ListShape
          .builder()
          .id("other#MyList")
          .member(
            MemberShape
              .builder()
              .id("other#MyList$member")
              .target("random#SomeStruct")
              .build()
          )
          .build()
      )
      mb.assemble().unwrap()
    }
    Assertions.assertEquals(
      CircularDependencyCheck(model),
      Some("random" :: "other" :: "random" :: Nil)
    )
  }

}
