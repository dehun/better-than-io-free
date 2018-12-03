package space.dehun

import cats._
import cats.effect._
import cats.free.Free
import cats.implicits._


sealed trait LogAction[A]

object LogAction {
  case class LogMsg(msg:String) extends LogAction[Unit]
}

object Log {
  def logMsg(msg:String) = Free.liftF(LogAction.LogMsg(msg))
}


