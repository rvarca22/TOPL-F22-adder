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
import Adder.Lang.Syntax (Expression (..), Program (..), Statement (..), BinaryOp(..), ExpVal(..), AugOp(..), Atom (..))
import Adder.Store (Store, deref, emptyStore, newref, setref)
import Data.Either (fromRight)
import GHC.Base (undefined)
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
resultOf (StmtList []) env st0 = return st0
resultOf (StmtList (stmt : stmts)) env st0 = do
  st1 <- resultOf stmt env st0
  resultOf (StmtList stmts) env st1
resultOf (PassStmt) env st = return st
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
valueOf (AtomExp (IdAtom var)) env st0 = ((deref addr st0), st0)
  where
    addr = applyEnv env var
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
    (IntVal n1, IntVal n2) -> IntVal(floor(fromIntegral n1 / fromIntegral n2))
    (IntVal n1, FloatVal n2) -> IntVal(floor(fromIntegral n1 / n2))
    (FloatVal n1, IntVal n2) -> IntVal(floor(n1 / fromIntegral n2))
    (FloatVal n1, FloatVal n2) -> IntVal(floor(n1 / n2))
    _ -> undefined
  Power -> case (val1, val2) of
    (IntVal n1, IntVal n2) -> IntVal(floor(fromIntegral n1 ** fromIntegral n2))
    (IntVal n1, FloatVal n2) -> FloatVal(fromIntegral n1 ** n2)
    (FloatVal n1, IntVal n2) -> FloatVal(n1 ** fromIntegral n2)
    (FloatVal n1, FloatVal n2) -> FloatVal(n1 ** n2)
  Mod -> case (val1, val2) of
    (IntVal n1, IntVal n2) -> IntVal(n1 `mod` n2)
    (IntVal n1, FloatVal n2) -> FloatVal(n1 `mod` n2)
    (FloatVal n1, IntVal n2) -> FloatVal(n1 `mod` n2)
    (FloatVal n1, FloatVal n2) -> FloatVal(n1 `mod` n2)
    _ -> undefined
  Divide -> FloatVal(n1 `div` n2)
  And -> BoolVal (x1 && x2)
  Or -> BoolVal (x1 && x2)
  _ -> error "unimplemented binary operation"
  where
    n1 = expvalToFloat val1
    n2 = expvalToFloat val2
    BoolVal x1 = val1
    BoolVal x2 = val2
  --Code here

--valueOf :: Return -> Environment -> Store -> Answer
--valueOf (Return exp1) env store = env2 --Added Exp 1 into the parathenses
--Answer Return exp1 env = exp1         --Attempted to add the return statement

-- valueOf(Return exp1)env = env1 exp2
---------------------------------------------
-- valueOf(exp1)env = env1 exp2
