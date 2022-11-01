{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides the lexical specification for Adder.
 -}
module Adder.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

adderLexer :: Tok.TokenParser ()
adderLexer =
  Tok.makeTokenParser adderLexSpec

adderLexSpec =
  emptyDef
    { Tok.commentLine = "#",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_",
      -- TODO Define the reserved operator symbols for the Adder language
      Tok.reservedOpNames = [],
      -- TODO Define the reserved names/keywords for the Adder language
      Tok.reservedNames = []
    }

integer :: Parser Integer
integer = Tok.integer adderLexer

symbol :: String -> Parser String
symbol = Tok.symbol adderLexer

parens :: Parser a -> Parser a
parens = Tok.parens adderLexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets adderLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep adderLexer

identifier :: Parser String
identifier = Tok.identifier adderLexer

reserved :: String -> Parser ()
reserved = Tok.reserved adderLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp adderLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace adderLexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral adderLexer
