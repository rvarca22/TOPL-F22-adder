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
import Adder.Lang.Syntax (Expression (..), Program (..), Statement (..), ExpVal (BoolVal))
import Adder.Store (Store, deref, emptyStore, newref, setref)
import Data.Either (fromRight)
import GHC.Base (undefined)
import Prelude hiding (exp)
import GHC.Float (stgWord32ToFloat)

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

resultOfStmts :: [Statement] -> Environment -> Store -> IO Store
resultOfStmts [] p st1 = return st1
resultOfStmts (stmt : stmts) p st1 = do
  st2 <- resultOf stmt p st1
  resultOfStmts stmts p st2


resultOf (IfStmt exp1 stmt1) p st0 = if q then st2 else st0
  where
    (BoolVal q, st1) = valueOf exp1 p st0
    st2 = resultOfStmts stmt1 p st1



resultOf (IfElseStmt exp1 trueStmts falseStmts) p st0 = if q then st2 else st3
  where
    (BoolVal q, st1) = valueOf exp1 p st0
    st2 = resultOfStmts trueStmts p st1
    st3 = resultOfStmts falseStmts p st1


resultOf (IfElifStmt exp1 stmt1 exp2 stmt2) p st0 = if q1 then st2 else if q2 then st4 else st0
  where
    (BoolVal q1, st1) = valueOf exp1 p st0
    st2 = resultOfStmts stmt1 p st1
    (BoolVal q2, st3) = valueOf exp2 p st0
    st4 = resultOfStmts p st3


resultOf (IfElifElseStmt exp1 stmt1 exp2 stmt2 stmt3) p st0 = if q1 then st2 else if q2 then st4 else st5
  where
    (BoolVal q1, st1) = valueOf exp1 p st0
    st2 = resultOfStmts stmt1 p st1
    (BoolVal q2, st3) = valueOf exp2 p st0
    st4 = resultOfStmts stmt2 p st3
    st5 = resultOfStmts stmt3 p st3


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
