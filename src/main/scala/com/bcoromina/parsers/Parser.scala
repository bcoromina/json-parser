package com.bcoromina.parsers

import scala.util.{Failure, Success, Try}


// parse function will return None if the content we expected is not there
// otherwise it will return Some((Failure, Int) if the expected content is there but an exception occurred while parsing.
// The mentioned exception should be a format error in the content.
case class Parser[+A](parse: (String,Int) => Option[(Try[A], Int)], tokenName: String = "unknown"){
  def map[B](f: A =>B): Parser[B] ={
    Parser( (s,i) =>
      parse(s,i) match{
        case None => None
        case Some((Success(a),n)) => Some((Success(f(a)), n))
        case Some((Failure(f),n)) => Some(Failure(f),n)
      },
      tokenName
    )
  }

  def withTokenName(word: String): Parser[A] = Parser(parse,word)

  def andThen[B](b: => Parser[B]): Parser[(A,B)] = {
    Parser((s, i) => {
      parse(s, i) match {
        case None => None
        case Some((Success(va), n)) =>
          b.parse(s, n) match {
            case Some((Success(vb), ni)) =>
              Some(Success((va,vb)), ni)
            case None =>
              Some(missingTokenFailure(b.tokenName, n+1), n +1)
            case Some((Failure(f), ni)) =>
              Some(Failure(f), ni)
          }
        case Some((Failure(f), i)) => Some(Failure(f), i)
      }
    },
      tokenName
    )
  }

  def flatMap[B](f: A => Parser[B]): Parser[B]= {
    Parser ( (s, i) => {
      parse(s, i) match {
        case None => None
        case Some((Success(va), n)) =>
          val lastParser = f(va)
          lastParser.parse(s, n) match {
            case Some((Success(vb), ni)) =>
              Some((Success(vb), ni))
            case None =>
              Some(missingTokenFailure(lastParser.tokenName, n+1), n +1)
            case Some((Failure(f), ni)) =>
              Some(Failure(f), ni)
          }
        case Some((Failure(f), i)) => Some(Failure(f), i)
      }
    },
      tokenName
    )
  }

  def parseString(s: String): Try[A] = {
    parse(s, 0) match {
      case None =>
        missingTokenFailure(tokenName, 1)
      case Some((Success(v), i)) if i == s.length =>
        Success(v)
      case Some((Success(_), i)) if i != s.length =>
        missingTokenFailure(tokenName, i + 1)
      case Some((f@Failure(_), _)) =>
        f
    }
  }

  private def missingTokenFailure(tokenName: String, pos: Int) = {
    if (tokenName != "") {
      Failure(new IllegalArgumentException(s"Missing token '${tokenName}' at position ${pos}"))
    } else {
      Failure(new Exception(s"Missing token at position $pos"))
    }
  }

}
object Parser{
  def tokenParser[A](token: String, jsonValue: A): Parser[A] = {
    Parser((s, i) => {
      if (s.startsWith(token, i)) {
        Some((Success(jsonValue), i + token.length))
      } else {
        None
      }
    },
      token
    )
  }

  implicit class ParserOps[A](a: Parser[A]) {
    def or(b: Parser[A]): Parser[A] = {
      Parser( (s,i) => {
        a.parse(s,i) match{
          case None => b.parse(s,i)
          case Some(r) => Some(r)
        }
      },
        s"${a.tokenName} or ${b.tokenName}"
      )
    }
    def tryOr(b: Parser[A]): Parser[A] = {
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
    }
  }

}


