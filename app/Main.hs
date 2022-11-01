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
import qualified Adder.Interp as Adder (interp)
import Control.Exception (ErrorCall, catch)
import Control.Monad (unless, void)
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import System.Environment (getArgs)
import System.IO (hPrint, stderr)

repl :: IO ()
repl = do
  runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "ADDER> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input ->
          unless (input == ":q") $
            liftIO (doInterp' Adder.interp input)
              >> loop

doInterp :: Show a => Interpreter a -> Source -> IO ()
doInterp interp input =
  case interp input of
    Left err -> print err
    Right val -> print val
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
      case Adder.interp prog of
        Left err -> print err
        Right st -> return ()
        `catch` (\e -> hPrint stderr (e :: ErrorCall))
      return ()
