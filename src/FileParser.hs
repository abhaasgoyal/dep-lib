{-# LANGUAGE OverloadedStrings #-}

module FileParser
  ( printOutput
  , parseInput
  ) where

import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           DepGraph                       ( Dependencies )
import           ErrorHandling                  ( Error(..) )


printOutput :: T.Text -> [T.Text] -> IO ()
printOutput parentDep childDep = do
  TIO.putStrLn $ parentDep <> " depends on " <> T.unwords childDep

-- | Check whether the string "depends on" appears on correct location
parseInput :: [[T.Text]] -> Either Error [Dependencies]
parseInput = traverse checkDepend
 where
  checkDepend :: [T.Text] -> Either Error Dependencies
  checkDepend s = case s of
    (parent : "depends" : "on" : children) ->
      Right (parent, S.fromList children)
    _ -> Left InvalidInputFile

