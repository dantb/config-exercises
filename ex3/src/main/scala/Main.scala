import config.*
import unindent.*
import cats.syntax.all.*
import cats.data.*
import cats.Show
import cats.derived.*

case class SystemConfig(server: ServerConfig, client: ClientConfig) derives Show
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

object Failure:
  given Show[Failure] = _ match
    case Failure.InvalidNum(ps)      => s"Invalid number at path: ${ps.mkString(", ")}"
    case Failure.InvalidStr(ps)      => s"Invalid string at path: ${ps.mkString(", ")}"
    case Failure.InvalidKey(key, ps) => s"Invalid key $key at path: ${ps.mkString(", ")}"

type Result[A] = ValidatedNel[Failure, A]
given [A]: Show[Result[A]] = r => r.fold(_.map(_.show).mkString_("\n"), _.toString)

trait Decoder[A]:
  def decode(config: Config): Result[A]

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
    case Config.Num(value) => Validated.Valid(value)
    case _                 => Validated.Invalid(NonEmptyList.of(Failure.InvalidNum(Nil)))

  given Decoder[String] = _ match
    case Config.Str(value) => value.validNel
    case _                 => Failure.InvalidStr(Nil).invalidNel

extension (conf: Config)
  def get[A: Decoder](key: String): Result[A] =
    conf.get(key).toValidNel(Failure.InvalidKey(key, List(key))).andThen(Decoder[A].decode(_).leftMap(_.map(_.withKey(key))))

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
  println(Decoder[SystemConfig].decode(config2).show)
