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
newtype Program
  = Pgm [Statement]
  deriving (Show)

-- TODO Build out the abstract syntax for Adder statements
data Statement
  = NoStmt -- TODO Remove this after adding legit data constructors
  deriving (Show)

-- TODO Build out the abstract syntax for Adder by adding more expressions
data Expression
  = UnaryExpr UnaryOp Expression
  | BinaryExpr BinaryOp Expression Expression
  | IntLiteralExp Integer
  | StringLiteralExp String
  | FloatLiteralExp Float
  deriving (Show)

data UnaryOp
  = Not
  | Negative
  deriving (Show)

-- TODO Build out the abstract syntax for more binary operations
data BinaryOp
  = Plus
  | Times
  | Power
  | Equal
  | Less
  | And
  | Is
  deriving (Show)

-- TODO Define more expressed values for the Adder language
data ExpVal
  = BoolVal Bool
  | IntVal Integer
  | FloatVal Float
  | StrVal String
  deriving (Eq)

-- TODO Implement "to-string" functionality for new Adder expressed values
instance Show ExpVal where
  show (BoolVal p) = show p
  show (IntVal p) = show p
  show (FloatVal p) = show p
  show (StrVal p) = show p
