import config.*
import unindent.*
import cats.syntax.all.*

case class SystemConfig(server: ServerConfig, client: ClientConfig)
object SystemConfig:
  given Decoder[SystemConfig] = Decoder.forProduct2("server", "client")(SystemConfig(_, _))

case class ServerConfig(version: String, host: String, port: Int)
object ServerConfig:
  given Decoder[ServerConfig] = Decoder.forProduct3("version", "host", "port")(ServerConfig(_, _, _))

case class ClientConfig(version: String, browser: String)
object ClientConfig:
  given Decoder[ClientConfig] = Decoder.forProduct2("version", "browser")(ClientConfig(_, _))

enum Failure(val path: List[String]):
  case InvalidNum(ps: List[String])              extends Failure(ps)
  case InvalidStr(ps: List[String])              extends Failure(ps)
  case InvalidKey(key: String, ps: List[String]) extends Failure(ps)

  def withKey(key: String): Failure = this match
    case InvalidNum(ps)    => InvalidNum(key :: ps)
    case InvalidStr(ps)    => InvalidStr(key :: ps)
    case InvalidKey(k, ps) => InvalidKey(k, key :: ps)

trait Decoder[A]:
  def decode(config: Config): Either[Failure, A]

object Decoder:
  def apply[A](using dec: Decoder[A]): Decoder[A] = dec

  def forProduct2[A: Decoder, B: Decoder, C](keyA: String, keyB: String)(f: (a: A, b: B) => C): Decoder[C] = conf =>
    (conf.get[A](keyA), conf.get[B](keyB)).mapN(f)

  def forProduct3[A: Decoder, B: Decoder, C: Decoder, D](
    keyA: String,
    keyB: String,
    keyC: String
  )(f: (a: A, b: B, c: C) => D): Decoder[D] = conf => (conf.get[A](keyA), conf.get[B](keyB), conf.get[C](keyC)).mapN(f)

  given Decoder[Int] = _ match
    case Config.Num(value) => Right(value)
    case _                 => Left(Failure.InvalidNum(Nil))

  given Decoder[String] = _ match
    case Config.Str(value) => Right(value)
    case _                 => Left(Failure.InvalidStr(Nil))

extension (conf: Config)
  def get[A: Decoder](key: String): Either[Failure, A] =
    conf.get(key).toRight(Failure.InvalidKey(key, List(key))).flatMap(Decoder[A].decode(_).leftMap(_.withKey(key)))

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
        browser = "Firefox"
      """
    )

  println(Decoder[SystemConfig].decode(config))
  println(Decoder[SystemConfig].decode(config2).leftMap(x => "Path to error: " ++ x.path.mkString(", ")))
