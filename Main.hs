module Main where

import Data.Maybe
import System (getArgs)
import System.Exit
import System.Console.GetOpt
import IO (hPutStrLn, stderr)

import DataFlow.Generic (labeling)
import While.Parser (loadFromFile)
import While.PrettyPrinter
import App.Sanity
import App.Slicing
import App.Optimizer
import Optimizer.DeadCode
import Optimizer.ConstFold

data Flag
     = Optimize | Slice Int | DeadCode | ConstFold | Verbose
       deriving Show

-- Command line options
options =
     [ Option "o" ["optimize"]  (NoArg Optimize) "optimize input file"
     , Option "d" ["deadcode"]  (NoArg DeadCode) "deadcode elimination"
     , Option "c" ["constfold"] (NoArg ConstFold) "constant folding"
     , Option "s" ["slice"]     (ReqArg slabel "LABEL") "program slice PoI LABEL"
     , Option "v" ["verbose"]   (NoArg Verbose) "verbose output"
     ]

slabel = Slice . read

-- Parse cmd line arguments
parse argv = case getOpt Permute options argv of
                      ([], [], errs) -> e errs
                      (_, [], errs) -> e errs
                      ([], _, errs) -> e errs
                      (o, n, []) -> return (o, n)
                      (_, _, _) -> e []
    where
      header = "Usage: Main OPTION file"
      e errs = do
        hPutStrLn stderr (concat errs ++ usageInfo header options)
        exitFailure

isVerbose Verbose = True
isVerbose _ = False
verboseOn = any isVerbose

main = do
  -- Parse command line arguments
  (as, fs) <- getArgs >>= parse

  -- Parse given input file
  p <- loadFromFile $ head fs
  case p of
    Nothing -> print "parse err"
    Just p' -> do {
                 if verboseOn as
                 then do
                   putStrLn "Original Code:"
                   pp lp
                   putStrLn "\nOutput:"
                 else
                   putStr ""
                 ;
                 if not $ null errors
                 then do
                   -- sanity check failed
                   mapM_ (putStrLn . ("[ERROR]: " ++)) errors
                   exitFailure
                 else
                     if not $ null as'
                     then
                         -- execute cmd line specified application
                         mapM_ method as'
                     else do
                       -- no app specified
                       putStrLn "[ERROR] Specify operation"
                       exitFailure
               }
      where
        -- perform sanity check on labeled program and obtain error
        -- messages (empty if program is sane)
        errors = sane lp
        method a = case a of
                     Slice pi -> print $ slice lp pi
                     Optimize -> pp $ optimize lp
                     DeadCode -> pp $ deadCode lp
                     ConstFold -> pp $ constFold lp
                     Verbose -> putStrLn ""
        as' = filter (not . isVerbose) as
        pp = putStrLn . printPretty
        lp = labeling p' -- label the program blocks
        print' (r, r') = do {p r; putStrLn ""; p r'}
            where
              p = mapM_ print
