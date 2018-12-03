package space.dehun

import cats.Monad
import cats._
import cats.implicits._
import cats.effect._
import cats.free.Free

sealed trait NetAction[A]

object NetAction {
  case class NotifyUserChange(user:User) extends NetAction[Unit]
}

object Net {
  def notifyUserChange(user:User)= Free.liftF(NetAction.NotifyUserChange(user))
}
