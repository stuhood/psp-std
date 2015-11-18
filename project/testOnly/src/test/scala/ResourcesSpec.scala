package psp
package tests

import psp.std._, api._

class ResourcesSpec extends ScalacheckBundle {
  def bundle = "Resources"

  def props: Direct[NamedProp] = Direct(
    s"psp contains resources         (testing classes on fs)"   -> Prop(resourceNames(path("psp")).nonEmpty),
    s"scala contains resources       (testing classes in jar)"  -> Prop(resourceNames(path("scala")).nonEmpty),
    s"null doesn't contain resources (testing exception)"       ->
      Prop(Prop.throws(classOf[UnsupportedOperationException])(resourceNames(path("null"))))
  )
}
