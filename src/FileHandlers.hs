module FileHandlers
  ( printOutput
  , parseInput
  ) where

import qualified Data.Set                      as S
                                                ( fromList )
import           DepParser                      ( Dependencies )
import           ErrorHandling


printOutput :: String -> [String] -> IO ()
printOutput parentDep childDep = do
  putStrLn $ parentDep ++ " depends on " ++ unwords childDep

-- printOutput :: Text -> [Text] -> IO ()
-- printOutput parentDep childDep = do
--   T.putStrLn $ parentDep <> " depends on " <> T.unwords childDep

-- | Check whether the string "depends on" appears on correct location
parseInput :: [[String]] -> Either Error [Dependencies]
parseInput = traverse checkDepend
 where
  checkDepend :: [String] -> Either Error Dependencies
  checkDepend s = case s of
    (parent : "depends" : "on" : children) ->
      Right (parent, S.fromList children)
    _ -> Left InvalidInputFile

