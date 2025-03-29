module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)


import CSV
import Interpreter

-- | Main entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        [queryFile] -> runQueryAndPrintResult queryFile
        _ -> do
            hPutStrLn stderr "Usage: cql <query-file>"
            exitFailure

-- | Run a query from a file and print the result
runQueryAndPrintResult :: FilePath -> IO ()
runQueryAndPrintResult queryFile = do
    result <- runQueryFile queryFile
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            exitFailure
        Right csv -> do
            -- Print the CSV as output
            putStrLn $ writeCSV csv
            exitSuccess

