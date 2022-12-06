{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides a reference implementation for an Adder interpreter.
 -}
module Adder.Interp
  ( interpWith,
    interpFile,
    interpInteractive,
    parseFile,
    parseInteractive,
  )
where

-- import Adder.Checker
import Adder.DataStructures (DenVal, Environment, ExpVal (..), Function (..))
-- import Adder.TypeEnv (TEnv (..), TypeEnvironment)
import Adder.Defs (Source)
import Adder.Environment (Env (..))
import Adder.Lang.Parser (ParseError, parseFile, parseInteractive)
import Adder.Lang.Syntax (Expression (..), Program (..), Statement (..))
import Adder.Store (Store, deref, emptyStore, newref, setref)
import Data.Either (fromRight)
import Prelude hiding (exp)
import GHC.Base (undefined)

type Interpreter a = a -> Environment -> Store -> IO Store

{- top-level interpreter routines -}

interpFile :: Source -> Either ParseError (IO Store)
interpFile src = interpWith resultOfProgram <$> parseFile src

-- interpInteractive :: Source -> Either ParseError (IO Store)
interpInteractive :: Source -> Either ParseError (IO Store)
interpInteractive src = interpWith resultOf <$> parseInteractive src

interpWith :: Interpreter a -> a -> IO Store
interpWith f x = f x emptyEnv emptyStore

{- semantic reduction of a program -}
-- TODO Implement the semantics for an Adder program

resultOfProgram :: Program -> Environment -> Store -> IO Store
resultOfProgram _ env st0 = undefined

{- semantic reductions for statements -}
-- TODO Implement the semantics for each kind of Adder statement

resultOf :: Statement -> Environment -> Store -> IO Store
resultOf _ env st0 = undefined

{- Evaluating a program yields an "answer" - a value and a resulting state. -}
type Answer = (ExpVal, Store)

{- semantic reductions for expressions -}

-- TODO Implement the semantics for each kind of Adder expression
valueOf :: Expression -> Environment -> Store -> Answer
valueOf _ env st0 = undefined
-- Binary Operation
valueOf (BinaryExp op exp₁ exp₂) ρ = valueOfBop op1 val₁ val₂
  where
    val₁ st1 = valueOf(exp₁) st0
    val₂ st2 = valueOf(exp₂) st1
    op1 = op

-- Don't forget about free store

{- Auxiliary functions -}
-- TODO Implement any helper functions needed to simplify the design of the
-- interpreter (e.g., the applyProcedure helper function).

valueOfBop :: BinaryOp -> ExpVal -> ExpVal -> ExpVal
valueOfBop op val₁ val₂ ρ = case op of
  _ -> undefined
  --Code here
  --where
    --variables defined here
