package com.agilogy.wpbtl.examples.main

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}


object JsonParser {

  def parse(s: String): Try[JsonValue] = {
    jsonValueP.parseString(s)
  }


  private def matchToken(s: String, token: String, index: Int): Boolean = {
    s.startsWith(token, index)
  }

  private def matchNumber(s: String, index: Int): Option[(Try[JsonValue],Int)] = {
    if( matchToken(s,"-", index) ||( index < s.length &&  s(index).isDigit ) ){
      @tailrec
      def loopNumber(s: String, i: Int, acc: List[Char]) : (Try[JsonValue], Int) ={
        if (s.length == i) {
          (Success(JsonNumber(acc.mkString)), i)
        } else {
          s(i) match {
            case c if c.isDigit || c == '.' || c == '-'  =>
              loopNumber(s, i + 1, acc :+ c)
            case _ =>
              (Success(JsonNumber(acc.mkString)), i)
          }
        }
      }
      Some(loopNumber(s,index, List.empty))
    }else{
      None
    }
  }

  private def matchString(s: String, index: Int): Option[(Try[JsonValue], Int)] = {
    if( matchToken(s,"\"", index)  ){

      def loopString(s: String, i: Int, acc: List[Char]): (Try[JsonValue], Int) = {
        if(s.length == i ){
          //end of string without "
          ( Failure(new IllegalArgumentException(s"Token missing \" ")), i)
        }else{
          s(i) match {
            case '\"' =>
              (Success(JsonString(acc.mkString)), i+1)
            case c =>
              loopString(s, i + 1, acc :+ c)
          }
        }

      }
      Some(loopString(s, index + 1, List.empty))
    }else{
      None
    }
  }


  val nullP: Parser[JsonValue] = Parser.tokenParser("null", JsonNull)
  val trueP: Parser[JsonValue] =  Parser.tokenParser("true", JsonBoolean(true))
  val falseP: Parser[JsonValue] = Parser.tokenParser("false", JsonBoolean(false))
  val stringP: Parser[JsonValue] = Parser(matchString, "string")
  val numberP: Parser[JsonValue] = Parser(matchNumber, "number")

  val openArrayParser: Parser[JsonValue] = Parser.tokenParser("[", JsonOpenArray)
  val closeArrayParser: Parser[JsonValue] = Parser.tokenParser("]", JsonCloseArray)
  val nextElementSepP: Parser[JsonValue] = Parser.tokenParser(",", JsonElementSep)

  private val openObjectP: Parser[JsonValue] = Parser.tokenParser("{", JsonOpenObject)
  private val closeObjectP: Parser[JsonValue] =  Parser.tokenParser("}", JsonCloseObject)
  private val memberSepP: Parser[JsonValue] = Parser.tokenParser(":", JsonMemberSep)



  lazy val jsonValueP: Parser[JsonValue] =
    (nullP or trueP or falseP or stringP or numberP or arrayP or objectP).withTokenName("jsonvalue")

  val elementsP2 : Parser[List[JsonValue]] = {
   (jsonValueP andThen nextElementSepP andThen elementsP2).map{ case ((v,_),e) => v :: e} tryOr jsonValueP.map(x => List(x))
  }.withTokenName("elements")

  lazy val elementsP : Parser[List[JsonValue]] = {
      {
        (for{
        v <- jsonValueP
        _ <- nextElementSepP
        e <- elementsP
      }yield{
        v :: e
      }).withTokenName("elements")
    } tryOr  jsonValueP.map(x => List(x))
  }.withTokenName("elements")

  lazy val arrayP: Parser[JsonValue]= {
    (for{
        _ <- openArrayParser
        rest <- closeArrayParser.map(_ => List.empty[JsonValue]) or {
          (for{
            e <- elementsP2
            _<- closeArrayParser
          }yield e)
        }
      }yield{
        JsonArray(rest).asInstanceOf[JsonValue]
      })
  }

  lazy val objectMember: Parser[(String, JsonValue)] ={
    (for{
      e <- stringP.asInstanceOf[Parser[JsonString]]
      _ <- memberSepP
      v <- jsonValueP
    }yield{
      (e.value -> v)
    }).withTokenName("objectMember")
  }

  lazy val objectMembers: Parser[List[(String, JsonValue)]] = {
    {
      {
        for {
          om <- objectMember
          _ <- nextElementSepP
          oms <- objectMembers
        } yield {
          oms:+om
        }
      } tryOr objectMember.map(x => List(x))
    }.withTokenName("objectMembers")
  }

  lazy val objectP : Parser[JsonValue] =
    (for{
     _ <- openObjectP
     v <- closeObjectP.map(_ => ListMap.empty[String,JsonValue]) or {
       for{
         e <- objectMembers
         _ <- closeObjectP
       }yield{
         ListMap.from(e)
       }
     }
   }yield{
     JsonObject(v)
   }).withTokenName("object")


}

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
            case Some((Success(vb), ni)) => Some(Success((va,vb)), ni)
            case None =>
              Some(missingTokenFailure(b.tokenName, n+1), n +1)
            case Some((Failure(f), ni)) => Some(Failure(f), ni)
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



