{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides a reference implementation for an Adder interpreter.
 -}
module Adder.Interp
  ( interpWith,
    interp,
  )
where

-- import Adder.Checker
import Adder.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
-- import Adder.TypeEnv (TEnv (..), TypeEnvironment)
import Adder.Defs (Source)
import Adder.Environment (Env (..))
import Adder.Lang.Parser (ParseError, parseToplevel)
import Adder.Lang.Syntax (Expression (..), Program (..), Statement (..))
import Adder.Store (Store, deref, emptyStore, newref, setref)
import Data.Either (fromRight)
import Prelude hiding (exp)

{- Evaluating a program yields an "answer" - a value and a resulting state. -}
data Answer = Answer {getVal :: ExpVal, getStore :: Store}

{- top-level interpreter routines -}
-- TODO Come back to this after we've discussed typed languages

-- checkAndInterp :: Source -> Either ParseError (IO Store)
-- checkAndInterp = checkAndInterpWith emptyTenv emptyEnv emptyStore

-- checkAndInterpWith :: TypeEnvironment -> Environment -> Store -> Source -> Either ParseError (IO Store)
-- checkAndInterpWith τ ρ σ src = flip (`resultOfProgram` ρ) σ <$> checkWith τ src

interp :: Source -> Either ParseError (IO Store)
interp = interpWith emptyEnv emptyStore

interpWith' :: Environment -> Store -> Source -> IO Store
interpWith' ρ σ = fromRight undefined . interpWith ρ σ

interpWith :: Environment -> Store -> Source -> Either ParseError (IO Store)
interpWith ρ σ src = flip (`resultOfProgram` ρ) σ <$> parseToplevel src

{- semantic reduction of a program -}
-- TODO Implement the semantics for an Adder program

resultOfProgram :: Program -> Environment -> Store -> IO Store
resultOfProgram (Pgm) ρ σ = undefined

{- semantic reductions for statements -}
-- TODO Implement the semantics for each kind of Adder statement

resultOf :: Statement -> Environment -> Store -> IO Store
resultOf (Stmt) ρ σ = undefined

{- semantic reductions for expressions -}
-- TODO Implement the semantics for each kind of Adder expression

valueOf :: Expression -> Environment -> Store -> Answer
valueOf (Exp) ρ σ = undefined

{- Auxiliary functions -}
-- TODO Implement any helper functions needed to simplify the design of the
-- interpreter (e.g., the applyProcedure helper function).
