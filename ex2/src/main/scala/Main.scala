import config.*
import unindent.*
import cats.syntax.all.*
import cats.data.*

case class SystemConfig(server: ServerConfig, client: ClientConfig)
object SystemConfig:
  given Decoder[SystemConfig] = Decoder.product2("server", "client")(SystemConfig(_, _))

case class ServerConfig(version: String, host: String, port: Int)
object ServerConfig:
  given Decoder[ServerConfig] = Decoder.product3("version", "host", "port")(ServerConfig(_, _, _))

case class ClientConfig(version: String, browser: String)
object ClientConfig:
  given Decoder[ClientConfig] = Decoder.product2("version", "browser")(ClientConfig(_, _))

enum Failure(val path: List[String]):
  case InvalidNum(ps: List[String])              extends Failure(ps)
  case InvalidStr(ps: List[String])              extends Failure(ps)
  case InvalidKey(key: String, ps: List[String]) extends Failure(ps)

  def withKey(key: String): Failure = this match
    case InvalidNum(ps)    => InvalidNum(key :: ps)
    case InvalidStr(ps)    => InvalidStr(key :: ps)
    case InvalidKey(k, ps) => InvalidKey(k, key :: ps)

// type Result[A] = EitherNel[]

trait Decoder[A]:
  def decode(config: Config): EitherNel[Failure, A]

object Decoder:
  def apply[A](using dec: Decoder[A]): Decoder[A] = dec

  def product2[A: Decoder, B: Decoder, C](keyA: String, keyB: String)(f: (a: A, b: B) => C): Decoder[C] = conf =>
    (conf.get[A](keyA), conf.get[B](keyB)).mapN(f)

  def product3[A: Decoder, B: Decoder, C: Decoder, D](
    keyA: String,
    keyB: String,
    keyC: String
  )(f: (a: A, b: B, c: C) => D): Decoder[D] = conf => (conf.get[A](keyA), conf.get[B](keyB), conf.get[C](keyC)).mapN(f)

  given Decoder[Int] = _ match
    case Config.Num(value) => value.rightNel
    case _                 => Failure.InvalidNum(Nil).leftNel

  given Decoder[String] = _ match
    case Config.Str(value) => value.rightNel
    case _                 => Failure.InvalidStr(Nil).leftNel

extension (conf: Config)
  def get[A: Decoder](key: String): EitherNel[Failure, A] =
    conf.get(key).toRightNel(Failure.InvalidKey(key, List(key))).flatMap(Decoder[A].decode(_).leftMap(_.map(_.withKey(key))))

// To accumulate errors - stick a Semigroup in the Left like a NEL
// To keep path, we need to thread in the decoding function then append the path when decoding at that point. Put this in the error msg.

@main def main(): Unit =
  val config: Config =
    ConfigParser.unsafeParse(
      i"""
      server:
        version = "2.1.0"
        host    = "localhost"
        port    = 8000
      client:
        version = "1.0.0"
        browser = "Firefox"
      """
    )

  val config2: Config =
    ConfigParser.unsafeParse(
      i"""
      server:
        version = "2.1.0"
        host    = "localhost"
        port    = 8000
      client:
        version = 3
        browser = 4
      """
    )

  println(Decoder[SystemConfig].decode(config))
  println(Decoder[SystemConfig].decode(config2).leftMap(_.map(x => "Path to error: " ++ x.path.mkString(", ")).mkString_("\n")))
