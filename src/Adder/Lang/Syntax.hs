{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides the abstract syntax representation for Adder.
 -}
module Adder.Lang.Syntax where

import Adder.Defs (Identifier)

-- TODO Build out the top-level abstract syntax for an Adder program
data Program
  = Pgm
  deriving (Eq, Ord, Show)

-- TODO Build out the abstract syntax for Adder statements
data Statement
  = Stmt
  deriving (Eq, Ord, Show)

-- TODO Build out the abstract syntax for Adder expressions
data Expression
  = Exp
  deriving (Eq, Ord, Show)
