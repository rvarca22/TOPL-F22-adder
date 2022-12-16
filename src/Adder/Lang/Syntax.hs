{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides the abstract syntax representation for Adder.
 -}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Adder.Lang.Syntax where

import Adder.Defs (Identifier)

-- TODO Build out the top-level abstract syntax for an Adder program
newtype Program
  = Pgm [Statement]
  deriving (Show)

-- TODO Build out the abstract syntax for Adder statements
data Statement
  = IfStmt Expression [Statement] -- If Statement contains Exprression and list of statements - at this time only one statement
  | ReturnStmt Expression -- I did this to return just an expression of any kind, so the user could use return for many purposes
  | PassStmt
  | StmtList [Statement]
  | AssignmentStmt Identifier Expression
  | AugmentedAssignmentStmt Identifier AugOp Expression -- Bashir's Augmented Assignment constructor
  | BreakStmt
  | ContinueStmt
  | WhileStmt Expression [Statement]
  deriving (Show)

-- TODO Build out the abstract syntax for Adder by adding more expressions
data Expression
  = UnaryExpr UnaryOp Expression
  | BinaryExpr BinaryOp Expression Expression
  | AtomExp Atom
  | IntLiteralExp Integer
  | StringLiteralExp String
  | FloatLiteralExp Float
  | BoolLiteralExp Bool
  deriving (Show)

-- Bashir's Augment assignment operations
data AugOp
    = AugPlus
    | AugMinus
    | AugMulti
    | AugDiv
    deriving (Show)

data UnaryOp
  = Negative
  | Positive
  | Not
  deriving (Show)

-- TODO Build out the abstract syntax for more binary operations
data BinaryOp
  = Power
  | Times
  | Plus
  | Minus
  | In
  | NotIn
  | Is
  | IsNot
  | Less
  | LessEqual
  | Greater
  | GreatEqual
  | NotEqual
  | Equal
  | And
  | Mod
  | IntDiv
  | Divide
  | Or
  deriving (Eq, Ord, Show)

-- TODO Define more expressed values for the Adder language
data ExpVal
  = BoolVal Bool
  | IntVal Integer
  | FloatVal Float
  | StrVal String
  deriving (Eq)

data Atom
  = IdAtom Identifier
  deriving (Show)
  --Will need Literals
  --Will need Enclosure

-- TODO Implement "to-string" functionality for new Adder expressed values
instance Show ExpVal where
  show (BoolVal p) = show p
  show (IntVal p) = show p
  show (FloatVal p) = show p
  show (StrVal p) = show p

expvalToFloat :: ExpVal -> Float
expvalToFloat val -> case val of
  IntVal val -> fromInteger val :: Float
  FloatVal val -> val
  StrVal val -> "This value cannot be converted to a float"
  BoolVal val -> "This value cannot be converted to a float"