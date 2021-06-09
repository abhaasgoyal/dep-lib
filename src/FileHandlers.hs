module FileHandlers
  ( printOutput
  , checkInput
  , extractDepsFromFile
  ) where

import qualified Data.Set                      as S
                                                ( fromList )
import qualified Data.Text                     as T
import           DepParser                      ( Dependencies )


printOutput :: String -> [String] -> IO ()
printOutput parentDep childDep = do
  putStrLn $ parentDep ++ " depends on " ++ unwords childDep

-- | Check whether the string "depends on" appears on correct location
checkInput :: [[String]] -> Bool
checkInput = all checkDepend
 where
  checkDepend :: [String] -> Bool
  checkDepend s | length s < 3 = False
                | otherwise = case s of
                    (_:"depends":"on":_) -> True
                    _ -> False
                | otherwise    = False

-- | Model dependencies read from the file
-- Predicate: Input is correct
extractDepsFromFile :: [[String]] -> [Dependencies]
extractDepsFromFile = map (\s -> (head s, S.fromList $ drop 3 s))
