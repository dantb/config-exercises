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

trait Decoder[A]:
  def decode(config: Config): Option[A]

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
    case Config.Num(value) => Some(value)
    case _                 => None

  given Decoder[String] = _ match
    case Config.Str(value) => Some(value)
    case _                 => None

extension (conf: Config)
  def get[A: Decoder](key: String): Option[A] =
    conf.get(key).flatMap(Decoder[A].decode)

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

  println(Decoder[SystemConfig].decode(config))

// given Applicative[Decoder] with
//   def pure[A](x: A): Decoder[A] = _ => Some(x)
//   def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]): Decoder[B] = conf =>
//     fa.decode(conf).flatMap(x => ff.decode(conf).map(f => f(x)))

// def decodeAsSystemConfig(config: Config): Option[SystemConfig] =
//   for
//     server <- decodeServerConf(config)
//     client <- decodeClientConf(config)
//   yield SystemConfig(server, client)

// def decodeServerConf(config: Config): Option[ServerConfig] =
//   config.get("server").flatMap { server =>
//     for
//       version <-server.getStr("version")
//       host <-server.getStr("host")
//       port <-server.getNum("port")
//     yield ServerConfig(version, host, port)
//   }

// def decodeClientConf(config: Config): Option[ClientConfig] =
//   config.get("client").flatMap { client =>
//     for
//       version <-client.getStr("version")
//       browser <-client.getStr("browser")
//     yield ClientConfig(version, browser)
//   }
