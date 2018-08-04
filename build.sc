import mill._, scalalib._
import ammonite.ops._

object wfc extends ScalaModule {
  def scalaVersion = "2.12.6"
  def unmanagedClasspath = T {
    if (!exists(millSourcePath / "lib")) Agg()
    else Agg.from(ls(millSourcePath / "lib").map(PathRef(_)))
  }

  def forkArgs = T(Seq("-Djava.library.path=" + (millSourcePath / "lib" / "native")))
}
