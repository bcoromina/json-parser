package com.bcoromina.jsonparser.test

import com.bcoromina.jsonparser.{JsonArray, JsonBoolean, JsonNull, JsonNumber, JsonObject, JsonOpenArray, JsonParser, JsonString, JsonValue}
import com.bcoromina.parsers.Parser
import com.bcoromina.parsers.ParserCombinators._
import org.scalatest.funsuite.AnyFunSuite


import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

class JsonParserTest extends AnyFunSuite{

  test("parse null"){
    assert(JsonParser.parse("null") == Success(JsonNull))
  }

  test("parse true") {
    assert(JsonParser.parse("true") == Success(JsonBoolean(true)))
  }

  test("parse false") {
    assert(JsonParser.parse("false") == Success(JsonBoolean(false)))
  }

  test("parse number") {
    assert(JsonParser.parse("3") == Success(JsonNumber("3")))
  }

  test("empty string") {
    assert(JsonParser.parse("").isFailure)
  }

  test("non empty string") {
    assert(JsonParser.parse(""""hola"""") == Success(JsonString("hola")))
  }

  test("non empty string containing token") {
    assert(JsonParser.parse(""""null"""") == Success(JsonString("null")))
  }

  test("token name propagation andThen") {
    val p = JsonParser.nullP andThen JsonParser.trueP
    p.parseString("nullfalse") match {
      case Failure(t) =>
        assert(t.getMessage == "Missing token 'true' at position 5")
      case _ => fail()
    }
  }

  test("token name propagation or") {
    val p = JsonParser.nullP or JsonParser.trueP
    p.parseString("hola") match {
      case Failure(t) =>
        assert(t.getMessage == "Missing token 'null or true' at position 1")
      case _ => fail()
    }
  }

  test("tryOr") {
    val p1 = JsonParser.nullP andThen JsonParser.trueP
    val p2 = JsonParser.nullP andThen JsonParser.falseP

    val p3 = p1 tryOr p2

    p3.parseString("nullfalse") match {
      case Success(value) =>
        assert(true)
      case _ =>
        fail()
    }
  }

  test("token name propagation in recursive combination") {
    val element = JsonParser.falseP or JsonParser.trueP

    lazy val elements: Parser[Unit] = {
       { //element with ontinuation
        val p = (for {
          _ <- element.map(_ => ())
          _ <- JsonParser.nextElementSepP
          _ <- elements
        } yield ())
          p
       } tryOr //element without continuation
         element.map(_ => ())
      }.withTokenName("elements")

    elements.parseString("true,") match {
      case Failure(t) =>
        assert(t.getMessage == "Missing token 'elements' at position 5")
      case _ =>
        fail()
    }
  }

  test("array with bad format") {
    val p = JsonParser.arrayP
    val res = p.parseString("[,]")
    res match {
      case Failure(t) =>
        assert(t.getMessage == "Missing token '] or elements' at position 2")
      case _ =>
        fail()
    }
  }

  test("array with missing closing") {
    val p = JsonParser.arrayP
    val res = p.parseString("[1,2")
    res match {
      case Failure(t) =>
        assert(t.getMessage == "Missing token ']' at position 5")
      case _ =>
        fail()
    }
  }

  test("missing word propagation or") {
    val p =  JsonParser.nullP or JsonParser.trueP
    p.parseString("false") match {
      case Failure(t) =>
        assert(t.getMessage == "Missing token 'null or true' at position 1")
      case _ =>
        fail()
    }
  }

  test("token name propagation tryOr") {
    val p1 =  JsonParser.nullP.andThen(JsonParser.trueP)
    val p2 =  JsonParser.nullP.andThen(JsonParser.falseP)

    val p3 = p1 tryOr p2

    p3.parseString("nullhola") match {
      case Failure(t) =>
      assert(t.getMessage == "Missing token 'false' at position 5")
      case _ =>
        ()
    }
  }


  test("parse array 1") {
    assert(JsonParser.parse("[true,false,null,true]") == Success(
      JsonArray(
        List(
          JsonBoolean(true),
          JsonBoolean(false),
          JsonNull,
          JsonBoolean(true)
        )
      )
    ))
  }




  test("empty array"){
    assert(JsonParser.parse("[]") == Success(JsonArray(List.empty)) )
  }


  ignore("ignores whitespaces") {
    assert(JsonParser.parse("[ true, false,null ,true] ") == Success(
      JsonArray(
        List(
          JsonBoolean(true),
          JsonBoolean(false),
          JsonNull,
          JsonBoolean(true)
        )
      )
    )
    )
  }

  ignore("formated json") {
    assert(JsonParser.parse(
      """ [
        |   true,
        |   "fa lse",
        |   null  ,
        |   true
        |] """.stripMargin) == Success(
      JsonArray(
        List(
          JsonBoolean(true),
          JsonString("fa lse"),
          JsonNull,
          JsonBoolean(true)
        )
      )
    )
    )
  }

  test("array of booleans"){
    assert(JsonParser.parse("[true,false,null,true]") == Success(
      JsonArray(
        List(
          JsonBoolean(true),
          JsonBoolean(false),
          JsonNull,
          JsonBoolean(true)
        )
      )
     )
    )
  }

  test("array of strings") {
    assert(JsonParser.parse("""["one","two","three"]""") == Success(
      JsonArray(
        List(
          JsonString("one"),
          JsonString("two"),
          JsonString("three"),
        )
      )
    )
    )
  }


  test("array of numbers") {
    assert(JsonParser.parse("""[123,-22,333.24]""") == Success(
      JsonArray(
        List(
          JsonNumber("123"),
          JsonNumber("-22"),
          JsonNumber("333.24"),
        )
      )
    )
    )
  }


  test("array of different types") {
    assert(JsonParser.parse("""["one",null,true]""") == Success(
        JsonArray(
          List(
            JsonString("one"),
            JsonNull,
            JsonBoolean(true),
          )
        )
      )
    )
  }

  test("empty object"){
    assert(
      JsonParser.parse("{}") ==
        Success(JsonObject(ListMap.empty))
    )
  }

  test("object with numbers"){
    assert(
      JsonParser.parse("""{"one":1,"two":22,"three":-333}""") ==
        Success(
          JsonObject(
            ListMap(
              ("one", JsonNumber("1")),
              ("two", JsonNumber("22")),
              ("three", JsonNumber("-333"))
            )
          )
        )
    )
  }

  test("process an object"){
    assert(
      JsonParser.parse("""{"one":"hola","two":null,"three":true}""") ==
        Success(
          JsonObject(
            ListMap(
              ("one", JsonString("hola")),
              ("two", JsonNull),
              ("three", JsonBoolean(true))
            )
          )
        )
    )
  }


  test("object with nested array") {
    assert(
      JsonParser.parse("""{"one":[true,null,{"1":"11","2":true},"hola"],"two":null,"three":true}""") ==
        Success(
          JsonObject(
            ListMap(
              ("one",JsonArray(
                      List(
                        JsonBoolean(true),
                        JsonNull,
                        JsonObject(ListMap(
                          ("1",JsonString("11")),
                          ("2",JsonBoolean(true))
                        )),
                        JsonString("hola") ))),
              ("two", JsonNull),
              ("three", JsonBoolean(true))
            )
          )
        )
    )
  }

  test("object with nested array 2") {
    val res = JsonParser.parse("""[true,false,["hola",null,"adeu"],true,true]""")
    println(res.get.asString())
  }


  ignore("Unexpected token at position"){
    val json = "[null,t]"
    val res: Try[JsonValue] = JsonParser.parse(json)
    res match {
      case Failure(t) => assert(t.getMessage == s"Missing token at position 7")
      case Success(_) => fail()
    }
  }

  ignore("Unfinished array ") {
    val json = "[null,]"
    val res: Try[JsonValue] = JsonParser.parse(json)
    res match {
      case Failure(t) =>
        assert(t.getMessage == s"Missing token at position 7")
      case Success(_) => fail()
    }
  }

  test("close array missing") {
    val json = """[null,true"""
    val res: Try[JsonValue] = JsonParser.parse(json)
    res match {
      case Failure(t) => assert(t.getMessage == s"Missing token ']' at position ${json.length + 1}")
      case Success(_) => fail()
    }
  }

  test("close object missing") {
    val json = """{"w":true"""
    val res: Try[JsonValue] = JsonParser.parse(json)
    res match {
      case Failure(t) => assert(t.getMessage == s"Missing token '}' at position ${json.length +1}")
      case Success(_) => fail()
    }
  }




}

