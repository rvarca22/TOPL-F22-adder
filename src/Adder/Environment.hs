{--
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides the interface for the Environment abstraction, an
 -  abstract data type representing symbol-to-value mappings.
 -}
module Adder.Environment (Env (..)) where

import Adder.DataStructures (Binding, DenVal, Environment (..), ExpVal (..), Function (..))
import Adder.Defs (Identifier)

{- Interface for an environment (symbol-to-value mapping) -}

class Env e where
  -- construct an emptyEnv environment
  emptyEnv :: e

  -- construct new environment from existing environment plus a new binding
  extendEnv :: Identifier -> DenVal -> e -> e

  -- extract from an environment the mapped value if search symbol is present
  applyEnv :: e -> Identifier -> DenVal

  -- construct new environment from existing environment plus new bindings
  extendEnv' :: [Identifier] -> [DenVal] -> e -> e

{- Implementation of environment interface using data structure representation -}

instance Env Environment where
  emptyEnv = EmptyEnvironment
  extendEnv = Environment
  applyEnv EmptyEnvironment name = nobinding name
  applyEnv (Environment name loc env) name'
    | name' == name = loc
    | otherwise = applyEnv env name'

  extendEnv' [] [] = id
  extendEnv' vars vals =
    extendEnv' (tail vars) (tail vals) . extendEnv (head vars) (head vals)

{- Auxiliary functions -}

nobinding :: Identifier -> a
nobinding = error . ("No binding found for \"" ++) . (++ "\"")
