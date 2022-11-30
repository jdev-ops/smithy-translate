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

package scala_js

import typings.vscode.{mod => vscode}

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js
import typings.vscode.mod.ProviderResult

object extension {

  @JSExportTopLevel("activate")
  def activate(context: vscode.ExtensionContext): Unit = {
    vscode.window.showInformationMessage(s"Formatter loaded")

    context.subscriptions.push(
      asDispose(
        vscode.languages.registerDocumentFormattingEditProvider(
          new vscode.DocumentFilter() {
            override val language = "smithy"
          },
          new vscode.DocumentFormattingEditProvider() {
            override def provideDocumentFormattingEdits(
                document: vscode.TextDocument,
                options: vscode.FormattingOptions,
                token: vscode.CancellationToken
            ): ProviderResult[js.Array[vscode.TextEdit]] = {
              val firstLine = document.lineAt(0)
              val lastLine = document.lineAt(document.lineCount - 1)
              val textRange = new vscode.Range(
                firstLine.range.start,
                lastLine.range.end
              )
              val formatted = SmithyFormatter.format(document.getText())
              js.Array[vscode.TextEdit](
                vscode.TextEdit.replace(textRange, formatted)
              )
            }

          }
        )
      )
    )
    ()
  }

  private def asDispose(
      disposable: vscode.Disposable
  ): typings.vscode.anon.Dispose = new typings.vscode.anon.Dispose() {
    def dispose(): Any = disposable.dispose()
  }
}
