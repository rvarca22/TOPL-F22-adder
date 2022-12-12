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
import GHC.Base (undefined)
import Prelude hiding (exp)

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
resultOf (StmtList []) env st0 = return st0
resultOf (StmtList (stmt : stmts)) env st0 = do
  st1 <- resultOf stmt env st0
  resultOf (StmtList stmts) env st1
resultOf (PassStmt) env st = return st
resultOf _ env st0 = undefined

{-
resultOf (IfStmt test conseq) p st = if q then st2
  where
    Answer (BoolVal q) st1 = valueOf test p st
    st2 = resultOf conseq p st1
-}


resultOf (IfElseStmt [exp1] [exp2] [exp3]) p st = if q then st2 else st3
  where
    (BoolVal q, st1) = valueOf exp1 p st
    st2 = resultOf exp2 p st2
    st1 = resultOf exp3 p st3


resultOf (IfElifStmt test conseq) p st = if q then st2 then if r then st4
  where
    Answer (BoolVal q) st1 = valueOf test p st
    st2 = resultOf conseq p st1
    Answer (BoolVal r) st3 = valueOf test r st
    st4 = resultOf conseq p st3
-}

resultOf (IfElifElseStmt test conseq) p st = if q then st2 then if r then st4 else st5
  where
    Answer (BoolVal q) st1 = valueOf test p st
    st2 = resultOf conseq p st1
    Answer (BoolVal r) st3 = valueOf test r st
    st4 = resultOf conseq p st3
    st5 = resultOf conseq p st4
-}

{- Evaluating a program yields an "answer" - a value and a resulting state. -}
type Answer = (ExpVal, Store)

{- semantic reductions for expressions -}

-- TODO Implement the semantics for each kind of Adder expression
valueOf :: Expression -> Environment -> Store -> Answer
valueOf _ env st0 = undefined
-- Binary Operation
--valueOf (BinaryExp op exp1 exp2) env st0 = valueOfBop op val1 val2
--  where
--    (val1, st1) = valueOf exp1  env st0
--    (val2, st2) = valueOf exp2 env st1

-- Don't forget about free store

--valueOF :: assignmentExpr ->  ??

{- Auxiliary functions -}
-- TODO Implement any helper functions needed to simplify the design of the
-- interpreter (e.g., the applyProcedure helper function).

--valueOfBop :: BinaryOp -> ExpVal -> ExpVal -> ExpVal
--valueOfBop op val1 val2 = case op of
--  _ -> error "unimplemented binary operation"
--Code here

--valueOf :: Return -> Environment -> Store -> Answer
--valueOf (Return exp1) env store = env2 --Added Exp 1 into the parathenses
--Answer Return exp1 env = exp1         --Attempted to add the return statement

-- valueOf(Return exp1)env = env1 exp2
---------------------------------------------
-- valueOf(exp1)env = env1 exp2
