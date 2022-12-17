{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides some basic type definitions.
 -}
module Adder.Defs where

import Text.ParserCombinators.Parsec.Error (ParseError)

-- Represent identifiers using a Haskell String
type Identifier = String

-- Source code is simply a text string
type Source = String

-- References are just integer indexes into the store
type Reference = Int

-- Interpreter takes source code and produces a value (of some type)
type Interpreter a = Source -> Either ParseError a