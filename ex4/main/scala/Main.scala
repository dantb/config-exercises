import cats.data.{EitherNec, NonEmptyChain}
import cats.syntax.all.*
import config.*
import unindent.*

case class User(email: String, password: String)
case class UserConfig(users: List[User])

def decodeAsUserConfig(config: Config): Result[UserConfig] =
  
  ???

@main def main(): Unit =
  val config: Config =
    Config.block(
      "1" -> Config.block(
        "email" -> Config.leaf("alice@example.com"),
        "password" -> Config.leaf("password"),
      ),
      "2" -> Config.block(
        "email" -> Config.leaf("bob@example.com"),
        "password" -> Config.leaf("supersecret"),
      ),
      "3" -> Config.block(
        "email" -> Config.leaf("charlie@example.com"),
        "password" -> Config.leaf("opensesame"),
      ),
    )

  println(decodeAsUserConfig(config))

// TODO: Debug the parser so this parses:
// ConfigParser.unsafeParse(
//   i"""
//   users:
//     count = 3
//     user1:
//       email    = "alice@example.com"
//       password = "password"
//     user2:
//       email    = "bob@example.com"
//       password = "supersecret"
//     user3:
//       email    = "charlie@example.com"
//       password = "opensesame"
//   """
// )
