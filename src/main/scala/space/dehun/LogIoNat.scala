package space.dehun

import cats.effect.IO
import cats.~>

class LogIoNat extends ~>[LogAction, IO] {
  override def apply[A](fa: LogAction[A]): IO[A] = fa match {
    case LogAction.LogMsg(msg) => IO {
      Console.println(s"log: $msg")
    }

  }
}

