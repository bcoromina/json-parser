Json parser based on parser combinators

Steps:

- Model Json domain using ADT following https://www.json.org/json-en.html
  It will allow us to represent a parsed json string as AST. JsonValue.scala
- Define the Parser class and combinators
- Build primitive json parsers and more complex ones composing them. JsonParser.scala