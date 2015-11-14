package psp

import std._, api._

package psys {
  final case class Pid(pid: Int) extends AnyVal with ForceShowDirect {
    def to_s = s"$pid"
  }
}

package object psys {
  implicit def orderPid: Order[Pid] = orderBy(_.pid)
}
