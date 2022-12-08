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
import Adder.Lang.Syntax (Expression (..), Program (..), Statement (..), ExpVal(IntVal), AugOp(..))
import Adder.Store (Store, deref, emptyStore, newref, setref)
import Data.Either (fromRight)
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

resultOf (AugmentedAssignmentStmt var AugPlus rhs) env st0 = return st2
  where
    address1 = applyEnv env var
    IntVal (rval1) = deref address1 st0
    (IntVal (rval2), st1) = valueOf rhs env st0
    rval3 = IntVal ( rval1 +  rval2)
    st2 = setref address1 rval3 st1

--resultOf (AugmentedAssignmentStmt var AugPlus rhs) env st0 = return st2
--  where
--    address1 = applyEnv env var
--    rval1 = deref address1 st0
--    (rval2, st1) = valueOf rhs env st0
--    rval3 = case (rval1, rval2) of
--        (Intval n1, IntVal n2) -> IntVal (n1 + n2)
--    st2 = setref address1 rval3 st1

resultOf (AugmentedAssignmentStmt var AugMinus rhs) env st0 = return st2
  where
    address1 = applyEnv env var
    IntVal (rval1) = deref address1 st0
    (IntVal (rval2), st1) = valueOf rhs env st0
    rval3 = IntVal ( rval1 -  rval2)
    st2 = setref address1 rval3 st1

resultOf (AugmentedAssignmentStmt var AugMulti rhs) env st0 = return st2
  where
    address1 = applyEnv env var
    IntVal (rval1) = deref address1 st0
    (IntVal (rval2), st1) = valueOf rhs env st0
    rval3 = IntVal ( rval1 *  rval2)
    st2 = setref address1 rval3 st1

resultOf (AugmentedAssignmentStmt var AugDiv rhs) env st0 = return st2
  where
    address1 = applyEnv env var
    IntVal (rval1) = deref address1 st0
    (IntVal (rval2), st1) = valueOf rhs env st0
    rval3 = IntVal (div rval1 rval2) -- floor and frominteger
    st2 = setref address1 rval3 st1

--resultOf (PassStmt) _ env st0 = env -- pass does not do anything so would env not cahgne?
-- where
--    env = env

-- resultOf(PassStmt) env0 = env1
---------------------------------------------
-- env1 = env0
{-
resultOf (IfStmt test conseq) p st = if q then st2 else st3
  where
    Answer (BoolVal q) st1 = valueOf test p st
    st2 = resultOf conseq p st1
    st3 = resultOf conseq p st2
-}

{- Evaluating a program yields an "answer" - a value and a resulting state. -}
type Answer = (ExpVal, Store)

{- semantic reductions for expressions -}


-- TODO Implement the semantics for each kind of Adder expression
valueOf :: Expression -> Environment -> Store -> Answer
valueOf _ env st0 = undefined

--valueOF :: assignmentExpr ->  ??

{- Auxiliary functions -}
-- TODO Implement any helper functions needed to simplify the design of the
-- interpreter (e.g., the applyProcedure helper function).


--valueOf :: Return -> Environment -> Store -> Answer
--valueOf (Return exp1) env store = env2 --Added Exp 1 into the parathenses
--Answer Return exp1 env = exp1         --Attempted to add the return statement 

-- valueOf(Return exp1)env = env1 exp2
---------------------------------------------
-- valueOf(exp1)env = env1 exp2
