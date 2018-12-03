package space.dehun

import cats.effect.IO
import cats.~>

class NetIoNat extends ~>[NetAction, IO] {
  override def apply[A](fa: NetAction[A]): IO[A] = fa match {
    case NetAction.NotifyUserChange(user) => IO {
      Console.println(s"net:user changed = $user")
    }
  }
}
