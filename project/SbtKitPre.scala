import sbt._
import sbt.Keys._
import sbt.Scoped.DefinableTask

object SbtKitPre {
  implicit class DefinableTaskWithRemove[A](val _t: DefinableTask[Seq[A]]) extends AnyVal {
    def -=(x: A): Setting[Task[Seq[A]]] = _t ~= (_ filterNot x.==)
  }

  implicit class SettingKeyWithRemove[A](val _s: SettingKey[Seq[A]]) extends AnyVal {
    def -=(x: A): Setting[Seq[A]] = _s ~= (_ filterNot x.==)
  }
}
