package psp

import std._, api._
//import com.jezhumble.javasysmon._
import squants.time._

package psys {
//  final class ProcInfo(val p: ProcessInfo) {
//    def command    = p.getCommand
//    def name       = p.getName
//    def owner      = p.getOwner
//    def parent     = Pid(p.getParentPid)
//    def pid        = Pid(p.getPid)
//    def resident   = p.getResidentBytes.size
//    def systemTime = p.getSystemMillis
//    def total      = p.getTotalBytes.size
//    def userTime   = p.getUserMillis
//  }
//  object sysmon extends com.jezhumble.javasysmon.JavaSysMon {
//    def freeMem  = physical.getFreeBytes
//    def totalMem = physical.getTotalBytes

//    def cpuFrequency: Frequency  = Hertz(cpuFrequencyInHz)
//    def ptable: Direct[ProcInfo] = processTable map (x => new ProcInfo(x)) sortBy (_.pid)
//    def ptree: OsProcess         = processTree
//    def pid: Pid                 = Pid(currentPid())
//    def kill(pid: Pid)           = killProcess(pid.pid)
//  }
  final case class Pid(pid: Int) extends AnyVal with ForceShowDirect {
    def to_s = s"$pid"
  }
}

package object psys {
  implicit def orderPid: Order[Pid] = orderBy(_.pid)
//  implicit def showProcInfo: Show[ProcInfo] = Show { p =>
//    import p._
//    s"$name $owner $parent $pid $resident/$total $systemTime/$userTime"
//  }
}
