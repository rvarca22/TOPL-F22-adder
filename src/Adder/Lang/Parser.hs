{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides the grammatical specification for Adder.
 -}
module Adder.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import Adder.Lang.Lexer
import Adder.Lang.Syntax (Expression (..), Program (..), Statement (..))
import Text.Parsec (ParseError, choice, eof, many, parse, sepBy, try)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

parseToplevel :: String -> Either ParseError Program
parseToplevel = parse (contents toplevel) "<stdin>"

toplevel :: Parser Program
toplevel = program

parseExp :: String -> Either ParseError Expression
parseExp = parse (contents expression) "<stdin>"

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

{- Grammar for the PROC language -}

program :: Parser Program
program = undefined

statement :: Parser Statement
statement =
  (choice . map try)
    -- TODO Fill-in the parse rules for the Adder grammar
    []

expression :: Parser Expression
expression =
  (choice . map try)
    -- TODO Fill-in the parse rules for the Adder grammar
    []
