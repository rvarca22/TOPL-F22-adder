{-
 -  Main.hs
 -
 -  Reference implementation of the toy languages from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides routines for executables based on HOPL languages.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import Adder.Defs
import qualified Adder.Interp as Adder (interpFile, interpInteractive, parseFile, parseInteractive)
import Control.Exception (ErrorCall, catch)
import Control.Monad (unless, void, when)
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import System.Environment (getArgs)
import System.IO (hPrint, stderr)
import Text.Parsec.Indent
import Text.Pretty.Simple

repl :: IO ()
repl = do
  runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "ADDER> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just ":q" -> return ()
        Just (':' : cmd) ->
          when
            (take 2 cmd == "p ")
            (liftIO (doInterp Adder.parseInteractive (drop 2 cmd)))
            >> loop
        Just input ->
          unless (input == ":q") $
            liftIO (doInterp' Adder.interpInteractive input)
              >> loop

-- unless (input == ":q") $
--   liftIO (doInterp Adder.interp input)
--     >> loop

doInterp :: Show a => Interpreter a -> Source -> IO ()
doInterp interp input =
  case interp input of
    Left err -> print err
    Right val -> pPrint val
    `catch` (\e -> hPrint stderr (e :: ErrorCall))

doInterp' :: Interpreter (IO a) -> Source -> IO ()
doInterp' interp input =
  case interp input of
    Left err -> print err
    Right val -> void val
    `catch` (\e -> hPrint stderr (e :: ErrorCall))

run :: IO ()
run = do
  args <- getArgs
  if null args
    then putStrLn "runadder: Missing source file name"
    else do
      prog <- readFile $ head args
      case Adder.interpFile prog of
        Left err -> print err
        Right st -> st >>= print
        `catch` (\e -> hPrint stderr (e :: ErrorCall))

runparse :: IO ()
runparse = do
  args <- getArgs
  if null args
    then putStrLn "parseadder: Missing source file name"
    else do
      prog <- readFile $ head args
      case Adder.parseFile prog of
        Left err -> print err
        Right pgm -> pPrint pgm
        `catch` (\e -> hPrint stderr (e :: ErrorCall))
