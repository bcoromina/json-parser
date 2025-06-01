package com.bcoromina.parsers

import com.bcoromina.jsonparser.JsonValue

import scala.util.{Failure, Success, Try}


// parse function will return None if the content we expected is not there
// otherwise it will return Some((Failure, Int) if the expected content is there but an exception occurred while parsing.
// The mentioned exception should be a format error in the content.
case class Parser[+A](parse: (String,Int) => Option[(Try[A], Int)], tokenName: String = "unknown"){

  def withTokenName(word: String): Parser[A] = Parser(parse,word)

  def parseString(s: String): Try[A] =
    parse(s, 0) match {
      case None =>
        Parser.missingTokenFailure(tokenName, 1)
      case Some((Success(v), i)) if i == s.length =>
        Success(v)
      case Some((Success(_), i)) if i != s.length =>
        Parser.missingTokenFailure(tokenName, i + 1)
      case Some((f@Failure(_), _)) =>
        f
    }
}
object Parser{
  def tokenParser[A <: JsonValue](token: String, jsonValue: A): Parser[A] =
    Parser((s, i) => {
      if (s.startsWith(token, i))
        Some((Success(jsonValue), i + token.length))
      else None
    },
      token
    )

  def missingTokenFailure(tokenName: String, pos: Int): Failure[Nothing] =
    if (tokenName != "")
      Failure(new IllegalArgumentException(s"Missing token '${tokenName}' at position ${pos}"))
    else
      Failure(new Exception(s"Missing token at position $pos"))
}

object ParserCombinators{
  implicit class ParserOps[A](a: Parser[A]) {
    def or(b: Parser[A]): Parser[A] =
      Parser((s, i) => {
        a.parse(s, i) match {
          case None => b.parse(s, i)
          case Some(r) => Some(r)
        }
      },
        s"${a.tokenName} or ${b.tokenName}"
      )

    def tryOr(b: Parser[A]): Parser[A] =
      Parser((s, i) => {
        a.parse(s, i) match {
          case None =>
            b.parse(s, i)
          case Some(r@(Success(_), _)) => Some(r)
          case Some((Failure(_), _)) => b.parse(s, i)
        }
      },
        s"${a.tokenName} or ${b.tokenName}"
      )

    def andThen[B](b: => Parser[B]): Parser[(A, B)] =
      Parser((s, i) => {
        a.parse(s, i) match {
          case None => None
          case Some((Success(va), n)) =>
            b.parse(s, n) match {
              case Some((Success(vb), ni)) =>
                Some(Success((va, vb)), ni)
              case None =>
                Some(Parser.missingTokenFailure(b.tokenName, n + 1), n + 1)
              case Some((Failure(f), ni)) =>
                Some(Failure(f), ni)
            }
          case Some((Failure(f), i)) => Some(Failure(f), i)
        }
      },
        a.tokenName
      )


    def flatMap[B](f: A => Parser[B]): Parser[B] =
      Parser((s, i) => {
        a.parse(s, i) match {
          case None => None
          case Some((Success(va), n)) =>
            val lastParser = f(va)
            lastParser.parse(s, n) match {
              case Some((Success(vb), ni)) =>
                Some((Success(vb), ni))
              case None =>
                Some(Parser.missingTokenFailure(lastParser.tokenName, n + 1), n + 1)
              case Some((Failure(f), ni)) =>
                Some(Failure(f), ni)
            }
          case Some((Failure(f), i)) => Some(Failure(f), i)
        }
      },
        a.tokenName
      )


    def map[B](f: A => B): Parser[B] =
      Parser((s, i) =>
        a.parse(s, i) match {
          case None => None
          case Some((Success(a), n)) => Some((Success(f(a)), n))
          case Some((Failure(f), n)) => Some(Failure(f), n)
        },
        a.tokenName
      )

  }
}