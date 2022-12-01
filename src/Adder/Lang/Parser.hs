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
program :: IParser Program
program = undefined

-- See https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-statement
statement :: IParser Statement
statement = undefined

-- See https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-suite
suite :: IParser [Statement]
suite = undefined

-- See https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-compound_stmt
compoundStmt :: IParser Statement
compoundStmt =
  (choice . map try)
    []

-- See https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-simple_stmt
simpleStmt :: IParser Statement
simpleStmt =
  (choice . map try)
    []

-- See https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-stmt_list
stmtList :: IParser Statement
stmtList = undefined

-- See https://docs.python.org/3/reference/expressions.html#operator-precedence
table :: [[Operator String () (IndentT Identity) Expression]]
table =
  [ [Prefix (reservedOp "-" >> return (UnaryExpr Negative))],
    [Infix (reservedOp "**" >> return (BinaryExpr Power)) AssocRight],
    [ Infix (reservedOp "*" >> return (BinaryExpr Times)) AssocLeft
    ],
    [ Infix (reservedOp "+" >> return (BinaryExpr Plus)) AssocLeft
    ],
    [ Infix (reservedOp "<" >> return (BinaryExpr Less)) AssocLeft
    ],
    [ Infix (reservedOp "==" >> return (BinaryExpr Equal)) AssocLeft
    ],
    [ Infix (reserved "is" >> return (BinaryExpr Is)) AssocLeft
    ],
    [Prefix (reserved "not" >> return (UnaryExpr Not))],
    [Infix (reserved "and" >> return (BinaryExpr And)) AssocLeft],
    [Infix (reserved "or" >> return (BinaryExpr Or)) AssocLeft]
  ]

atom :: IParser Expression
atom = AtomExp <$> Id
  -- (choice . map try)
  -- [
  --   -- atom  ::=  identifier | literal | enclosure
  --   AtomExp <$> Id
  --   --AtomExp <$> literal,
  --   --AtomExp <$> enclosure
  -- ]

-- See https://docs.python.org/3/reference/expressions.html
expression :: IParser Expression
expression = buildExpressionParser table atom
  <?> "expression"

-- See https://docs.python.org/3/reference/expressions.html#grammar-token-python-grammar-atom
-- expression = 
--   (choice . map try)
--   [
--     --ExpVal <$> identifier
--   ]
  -- literal ::=  stringliteral | bytesliteral | integer | floatnumber | imagnumber
  -- Others should be handling something similar to this
-- literal = 
--   (choice . map try)
--   [
--     lit <$> string,
--     lit <$> integer,
--     lit <$> float
--   ]
  -- enclosure ::=  parenth_form | list_display | dict_display | set_display | generator_expression | yield_atom
-- enclosure = 
--   (choice . map try)
--   [
--     enc <$> parenth_form,
--     enc <$> list_display,
--     enc <$> dict_display,
--     enc <$> set_display,
--     enc <$> generator_expression,
--     enc <$> yield_atom
--   ]

  --yield_atom       ::=  "(" yield_expression ")"
  --yield_expression ::=  "yield" [expression_list | "from" expression]
-- yield_atom = symbol "(" >> yield_expression >* symbol ")"

-- yield_expression =
--   (choice . map try)
--   [
--     yield_exp <$> (reserved "yield" >> expression_list)
--     yield_exp <$> (reserved "yield" >> reserved "from" >> expression_list)
--   ]

