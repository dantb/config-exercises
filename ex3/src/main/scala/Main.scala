import config.*
import unindent.*
import cats.syntax.all.*
import cats.data.*
import cats.Show
import cats.derived.*
import cats.MonadThrow
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.MonadError
import cats.Parallel

case class SystemConfig(server: ServerConfig, client: ClientConfig) derives Show
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

object Failure:
  given Show[Failure] = _ match
    case Failure.InvalidNum(ps)      => s"Invalid number at path: ${ps.mkString(", ")}"
    case Failure.InvalidStr(ps)      => s"Invalid string at path: ${ps.mkString(", ")}"
    case Failure.InvalidKey(key, ps) => s"Invalid key $key at path: ${ps.mkString(", ")}"

type Error = NonEmptyList[Failure]
type Result[A] = EitherT[IO, Error, A]

// This allows us to accumulate errors based on the Either, not the IO.
given Parallel[Result] = EitherT.accumulatingParallel

def mkError(f: Failure): Error = NonEmptyList.of(f)

extension [A](res: Result[A])
  def run: Either[NonEmptyList[Failure], A] = runResult(res)

def runResult[A](res: Result[A]): Either[NonEmptyList[Failure], A] = res.value.unsafeRunSync()

trait Decoder[A]:
  def decode(config: Config): Result[A]

object Decoder:
  def apply[A](using dec: Decoder[A]): Decoder[A] = dec

  def product2[A: Decoder, B: Decoder, C](keyA: String, keyB: String)(f: (a: A, b: B) => C): Decoder[C] = conf =>
    (conf.get[A](keyA), conf.get[B](keyB)).parMapN(f)

  def product3[A: Decoder, B: Decoder, C: Decoder, D](
    keyA: String,
    keyB: String,
    keyC: String
  )(f: (a: A, b: B, c: C) => D): Decoder[D] = conf => (conf.get[A](keyA), conf.get[B](keyB), conf.get[C](keyC)).parMapN(f)

  given Decoder[Int] = _ match
    case Config.Num(value) => value.pure
    case _                 => MonadError[Result, Error].raiseError(mkError(Failure.InvalidNum(Nil)))

  given Decoder[String] = _ match
    case Config.Str(value) => value.pure
    case _                 => MonadError[Result, Error].raiseError(mkError(Failure.InvalidStr(Nil)))

extension (conf: Config)
  def get[A: Decoder](key: String): Result[A] =
    conf.get(key) match
      case None => MonadError[Result, Error].raiseError(mkError(Failure.InvalidKey(key, List(key))))
      case Some(value) => Decoder[A].decode(value).leftMap(_.map(_.withKey(key)))

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

  println(Decoder[SystemConfig].decode(config).run)
  println(Decoder[SystemConfig].decode(config2).run)
