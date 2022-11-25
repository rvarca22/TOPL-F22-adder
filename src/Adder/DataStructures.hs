{--
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides types for representing the values and other
 -  supporting data structures in Adder.
 -}
module Adder.DataStructures
  ( ExpVal (..),
    DenVal,
    StoVal,
    Binding,
    Environment (..),
    Function (..),
  )
where

import Adder.Defs (Identifier, Reference)
import Adder.Lang.Syntax (ExpVal, Expression, Statement)

-- Storable values are any expressed value
type StoVal = ExpVal

-- Denoted values are any expressed value
type DenVal = Reference

{- Recursive "data structure" representation for environments -}

type Binding = (Identifier, DenVal)

-- TODO Reimplement this based on a ribcage representation
data Environment = EmptyEnvironment | Environment Identifier DenVal Environment
  deriving (Eq)

instance Show Environment where
  show env = show $ envToList env

{- Auxiliary functions -}

envToList :: Environment -> [Binding]
envToList EmptyEnvironment = []
envToList (Environment x v savedEnv) = (x, v) : envToList savedEnv

{- Representation of closed procedure (i.e. closure) -}

-- TODO Fix the implementation of un-closed procedures.
data Function
  = ClosedFunction {funcParams :: [Identifier], funcBody :: [Statement], funcEnv :: Environment}
  | UnclosedFunction {funcParams :: [Identifier], funcBody :: [Statement]}
  deriving (Show)
