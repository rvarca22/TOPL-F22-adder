{-# LANGUAGE FlexibleContexts #-}

{-
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides the grammatical specification for Adder.
 -}
module Adder.Lang.Parser
  ( parseFile,
    parseInteractive,
    ParseError,
  )
where

import Adder.Lang.Lexer
import Adder.Lang.Syntax
import Adder.Lang.Syntax (Atom (IdAtom))
import Control.Monad (liftM2)
import Data.Functor.Identity (Identity)
import Text.Parsec hiding (parse, string)
import Text.Parsec.Expr
import Text.Parsec.Indent

parseFile :: String -> Either ParseError Program
parseFile = parse (contents program) "<stdin>"

parseInteractive :: String -> Either ParseError Statement
parseInteractive = parse (contents statement) "<stdin>"

parse :: (Stream s (IndentT Identity) t) => IndentParserT s () Identity a -> SourceName -> s -> Either ParseError a
parse p = runIndentParser p ()

contents :: IParser a -> IParser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

{- Grammar for the Adder language -}

-- TODO Implement a small but useful subset of the Python grammar
-- See https://docs.python.org/3/reference/grammar.html

-- See https://docs.python.org/3/reference/toplevel_components.html#complete-python-programs

-- block :: IParser statement -> IParser [statement]

program :: IParser Program
program = Pgm <$> block statement

-- See https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-statement
statement :: IParser Statement
statement =
  (choice . map try)
    [ compoundStmt,
      stmtList
    ]

-- See https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-suite
suite :: IParser [Statement]
suite = block statement <|> block stmtList

-- See https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-compound_stmt
compoundStmt :: IParser Statement
compoundStmt =
  (choice . map try)
    -- if_stmt ::=  "if" assignment_expression ":" suite   - If statement only
    [ IfStmt
        <$> (reserved "if" >> assignmentExpr)
        <*> (reservedOp ":" >> suite)
    ]

-- See https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-simple_stmt
simpleStmt :: IParser Statement
simpleStmt =
  (choice . map try)
    [ reserved "pass" >> return PassStmt, -- pass_stmt ::= "pass"
      (reserved "continue" >> return ContinueStmt),
      ReturnStmt <$> (reserved "return" >> expression), -- (reserved "return" >> [Expression]) -- Attempted to make it like the IsZero expression after feedback
      -- Attempted EBNF rule return_stmt ::=  "return" [expression_list]
      AssignmentStmt
        <$> identifier
        <*> (reservedOp "=" >> expression),
      AugmentedAssignmentStmt
        <$> identifier
        <*> augAssStmt -- parse the augmented operator here
        <*> expression, -- then parse the expression
      BreakStmt <$ reserved "BreakStmt"
    ]

-- See https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-stmt_list
stmtList :: IParser Statement
stmtList =
  (choice . map try)
    [ StmtList
        <$> sepBy simpleStmt (symbol ";")
    ]

-- Implementation of modulo
-- EBNF Rule: operator ::== "%"

-- Implementation of integer division
-- EBNF Rule: operator ::== "//"

-- Implementation of division
-- EBNF Rule: operator ::== "/"

-- See https://docs.python.org/3/reference/expressions.html#operator-precedence
table :: [[Operator String () (IndentT Identity) Expression]]
table =
  [ [Infix (reservedOp "**" >> return (BinaryExpr Power)) AssocRight],
    [ Prefix (reservedOp "-" >> return (UnaryExpr Negative)),
      Prefix (reservedOp "+" >> return (UnaryExpr Positive))
    ],
    [ Infix (reservedOp "*" >> return (BinaryExpr Times)) AssocLeft,
      Infix (reservedOp "/" >> return (BinaryExpr Divide)) AssocLeft,
      Infix (reservedOp "//" >> return (BinaryExpr IntDiv)) AssocLeft,
      Infix (reservedOp "%" >> return (BinaryExpr Mod)) AssocLeft
    ],
    [ Infix (reservedOp "+" >> return (BinaryExpr Plus)) AssocLeft
    ,
     Infix (reservedOp "-" >> return (BinaryExpr Minus)) AssocLeft
    ],
    [ Infix (reservedOp "in" >> return (BinaryExpr In)) AssocLeft,
      Infix (reservedOp "not in" >> return (BinaryExpr NotIn)) AssocLeft,
      Infix (reserved "is" >> return (BinaryExpr Is)) AssocLeft,
      Infix (reservedOp "is not" >> return (BinaryExpr IsNot)) AssocLeft,
      Infix (reservedOp "<" >> return (BinaryExpr Less)) AssocLeft,
      Infix (reservedOp "<=" >> return (BinaryExpr LessEqual)) AssocLeft,
      Infix (reservedOp ">" >> return (BinaryExpr Greater)) AssocLeft,
      Infix (reservedOp ">=" >> return (BinaryExpr GreatEqual)) AssocLeft,
      Infix (reservedOp "!=" >> return (BinaryExpr NotEqual)) AssocLeft,
      Infix (reservedOp "==" >> return (BinaryExpr Equal)) AssocLeft
    ],
    [Prefix (reserved "not" >> return (UnaryExpr Not))],
    [Infix (reserved "and" >> return (BinaryExpr And)) AssocLeft],
    [Infix (reserved "or" >> return (BinaryExpr Or)) AssocLeft]
  ]

-- See https://docs.python.org/3/reference/expressions.html
expression :: IParser Expression
expression =
  buildExpressionParser table atom
    <?> "expression"

-- assignment_expression ::=  [identifier ":="] expression
assignmentExpr :: IParser Expression
assignmentExpr = expression -- For now, assignment expression only needs to be an expression
-- (choice . map try)
--  [<$> identifier
--    <*> (reservedOp "=" >> Expression)]

-- See https://docs.python.org/3/reference/expressions.html#grammar-token-python-grammar-atom
atom :: IParser Expression
atom =
  AtomExp . IdAtom <$> identifier
    <|> IntLiteralExp <$> integer
    <|> StringLiteralExp <$> string
    <|> FloatLiteralExp <$> float
    <?> "atom"

-- (choice . map try)
-- [
--   -- atom  ::=  identifier | literal | enclosure
--   AtomExp <$> Id
--   --AtomExp <$> literal,
--   --AtomExp <$> enclosure
-- ]
