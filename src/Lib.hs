module Lib (
    introMessage,
    runQuery
  ) where

import CSV (CSV, readCSVFile, writeCSVFile, sortCSV)

introMessage :: String
introMessage =
  "CSV Query Language Interpreter"

-- | Run a query on CSV files and write the output
-- This is just a placeholder for now
runQuery :: String -> [FilePath] -> FilePath -> IO (Either String ())
runQuery _query _inputFiles outputFile = do
  -- Placeholder implementation:
  -- Just outputs an empty CSV file
  writeCSVFile outputFile []