import mill._, mill.scalalib._, mill.scalajslib._
import $ivy.`com.github.lolgab::mill-scalablytyped::0.1.0`
import com.github.lolgab.mill.scalablytyped._

object `scalablytyped-module` extends ScalaJSModule with ScalablyTyped {
  def scalaVersion = "2.13.10"
  def scalaJSVersion = "1.11.0"
}
