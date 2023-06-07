package config

import cats.syntax.all.*
// import cats.derived.*
import cats.kernel.Semigroup

enum Config:
  case Block(fields: Map[String, Config])
  case Str(value: String)
  case Num(value: Int)

  // returns sub config at this path
  def get(path: String | List[String]): Option[Config] =
    val expanded: List[String] =
      path match
        case path: String       => List(path)
        case path: List[String] => path

    expanded match
      case head :: tail =>
        this match
          case Block(fields) => fields.get(head).flatMap(_.get(tail))
          case _             => None

      case Nil =>
        Some(this)

  def getStr(key: String): Option[String] = this match
    case Block(fields) =>
      fields.collectFirst {
        case (k, value) if key == k =>
          value match
            case Str(v) => Some(v)
            case _      => None
      }.flatten
    case _             => None

  def getNum(key: String): Option[Int] = this match
    case Block(fields) =>
      fields.collectFirst {
        case (k, value) if key == k =>
          value match
            case Num(v) => Some(v)
            case _      => None
      }.flatten
    case _             => None

object Config:
  def block(fields: (String, Config)*): Block =
    Block(fields.toMap)

  def leaf(value: String | Int): Config =
    value match
      case value: String => Str(value)
      case value: Int    => Num(value)
