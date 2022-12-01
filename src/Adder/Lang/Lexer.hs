{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides the lexical specification for Adder.
 -}
module Adder.Lang.Lexer where

import Data.Functor.Identity (Identity)
import Text.Parsec (alphaNum, char, letter, oneOf, spaces, (<|>))
import Text.Parsec.Indent (IndentParser, IndentT)
import Text.Parsec.Token (GenLanguageDef (LanguageDef))
import qualified Text.Parsec.Token as Tok

type IParser a = IndentParser String () a

adderLexer :: Tok.GenTokenParser String u (IndentT Identity)
adderLexer =
  Tok.makeTokenParser adderLexSpec

adderLexSpec :: Tok.GenLanguageDef String u (IndentT Identity)
adderLexSpec =
  LanguageDef
    { Tok.caseSensitive = True,
      Tok.nestedComments = False,
      Tok.commentStart = "",
      Tok.commentEnd = "",
      -- See https://docs.python.org/3/reference/lexical_analysis.html#comments
      Tok.commentLine = "#",
      -- See https://docs.python.org/3/reference/lexical_analysis.html#identifiers
      Tok.identStart = letter <|> char '_',
      Tok.identLetter = alphaNum <|> oneOf "_",
      -- TODO Define the reserved names/keywords for the Adder language
      -- See https://docs.python.org/3/reference/lexical_analysis.html#keywords
      Tok.reservedNames =
        ["pass", "return", "if", "or", "and", "is", "not", "in", "not in", "is not"],
      -- TODO Define the reserved operator symbols for the Adder language
      -- See https://docs.python.org/3/reference/lexical_analysis.html#operators
      Tok.opStart = Tok.opLetter adderLexSpec,
      Tok.opLetter = oneOf "+-*/%@<>&|^~:=!.",
      Tok.reservedOpNames = 
        ["+=", "-=", "*=", "/=", "**", "-", "*", "%", "//", "/", "+", "<", "<=", ">", ">=", "!=", "=="]
    }
  
boolean :: IParser Bool
boolean = trueLiteral <|> falseLiteral
  where
    trueLiteral = reserved "True" >> return True
    falseLiteral = reserved "False" >> return False

augAssStmt :: IParser Statement -- Bashir's augmented assignment function for different operations
augAssStmt =
  reserved "+=" >> return AugPlus
  <|> reserved "-=" >> return AugMinus
  <|> reserved "*=" >> return AugMulti
  <|> reserved "/=" >> return AugDiv

-- integer :: Parser Integer
integer :: IParser Integer
integer = Tok.integer adderLexer

float :: IParser Float
float = realToFrac <$> Tok.float adderLexer

string :: IParser String
string = Tok.stringLiteral adderLexer

symbol :: String -> IParser String
symbol = Tok.symbol adderLexer

parens :: IParser a -> IParser a
parens = Tok.parens adderLexer

brackets :: IParser a -> IParser a
brackets = Tok.brackets adderLexer

commaSep :: IParser a -> IParser [a]
commaSep = Tok.commaSep adderLexer

semiSep1 :: IParser a -> IParser [a]
semiSep1 = Tok.semiSep1 adderLexer

identifier :: IParser String
identifier = do
  s <- Tok.identifier adderLexer
  spaces
  return s

reserved :: String -> IParser ()
reserved = Tok.reserved adderLexer

reservedOp :: String -> IParser ()
reservedOp = Tok.reservedOp adderLexer

whiteSpace :: IParser ()
whiteSpace = Tok.whiteSpace adderLexer
