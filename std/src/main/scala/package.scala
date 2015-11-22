package psp

package object std extends psp.std.StdPackageObject {
  final val NoIndex       = Index.invalid
  final val NoFile: jFile = jFile("")
  final val NoPath: jPath = jPath("")
  final val NoUri: jUri   = jUri("")
}
