module FileHandlers
  ( printOutput
  , checkInput
  , extractDepsFromFile
  ) where

import qualified Data.Set                      as S
                                                ( fromList )
import           DepParser                      ( Dependencies )

printOutput :: String -> [String] -> IO ()
printOutput parentDep childDep = do
  putStrLn $ parentDep ++ " depends on " ++ unwords childDep

-- | Check whether the string "depends on" appears on correct location
checkInput :: [[String]] -> Bool
checkInput = all helpCheckDepend
 where
  helpCheckDepend :: [String] -> Bool
  helpCheckDepend s | length s < 4                         = True
                    | (s !! 1) ++ (s !! 2) == "depends on" = True
                    | otherwise                            = False

-- | Model dependencies read from the file
extractDepsFromFile :: [[String]] -> [Dependencies]
extractDepsFromFile = map (\s -> (head s, S.fromList $ drop 3 s))
