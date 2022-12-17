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
import Adder.Lang.Parser (ParseError, parseFile, parseInteractive,)
import Adder.Lang.Syntax (Expression (..), Program (..), Statement (..), ExpVal (..))

import Adder.Store (Store, deref, emptyStore, newref, setref)
import Data.Either (fromRight)
import Prelude hiding (exp)
import Adder.Lang.Syntax(BinaryOp (Plus, Minus, Times, FloorDiv, Power))

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

{- Auxiliary functions -}
-- TODO Implement any helper functions needed to simplify the design of the
-- interpreter (e.g., the applyProcedure helper function).
valueOfBop :: BinaryOp -> ExpVal -> ExpVal -> ExpVal
valueOfBop op val1 val2 = case op of
  Plus -> case (val1, val2) of
    (IntVal n1, IntVal n2) -> IntVal(n1 + n2)
    (IntVal n1, FloatVal n2) -> FloatVal(fromIntegral n1 + n2)
    (FloatVal n1, IntVal n2) -> FloatVal(n1 + fromIntegral n2)
    (FloatVal n1, FloatVal n2) -> FloatVal(n1 + n2)
    (StrVal s1, StrVal s2) -> StrVal(s1 ++ s2)
    _ -> undefined
  Minus -> case (val1, val2) of
    (IntVal n1, IntVal n2) -> IntVal(n1 - n2)
    (IntVal n1, FloatVal n2) -> FloatVal(fromIntegral n1 - n2)
    (FloatVal n1, IntVal n2) -> FloatVal(n1 - fromIntegral n2)
    (FloatVal n1, FloatVal n2) -> FloatVal(n1 - n2)
    _ -> undefined
  Times -> case (val1, val2) of
    (IntVal n1, IntVal n2) -> IntVal(n1 * n2)
    (IntVal n1, FloatVal n2) -> FloatVal(fromIntegral n1 * n2)
    (FloatVal n1, IntVal n2) -> FloatVal(n1 * fromIntegral n2)
    (FloatVal n1, FloatVal n2) -> FloatVal(n1 * n2)
    _ -> undefined
  FloorDiv -> case (val1, val2) of
    (IntVal n1, IntVal n2) -> IntVal(floor(fromIntegral n1 // fromIntegral n2))
    (IntVal n1, FloatVal n2) -> IntVal(floor(fromIntegral n1 // n2))
    (FloatVal n1, IntVal n2) -> IntVal(floor(n1 // fromIntegral n2))
    (FloatVal n1, FloatVal n2) -> IntVal(floor(n1 // n2))
    _ -> undefined
  Power -> case (val1, val2) of
    (IntVal n1, IntVal n2) -> IntVal(floor(fromIntegral n1 ** fromIntegral n2))
    (IntVal n1, FloatVal n2) -> FloatVal(floor(fromIntegral n1 ** n2))
    (FloatVal n1, IntVal n2) -> FloatVal(n1 ** fromIntegral n2)
    (FloatVal n1, FloatVal n2) -> FloatVal(n1 ** n2)
    _ -> undefined