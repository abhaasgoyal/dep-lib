module Main where
import           Control.Monad
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Set                      as S
import           DepParser
import           System.Environment
import           System.Exit

data Error =
    InvalidParam
  | CircularDependency
  | InvalidInput
  deriving (Eq)

-- | Driver function
main :: IO ()
main = do
  -- Get arguments
  args <- getArgs
  when (length args /= 1) $ handleError InvalidParam

  -- Read file
  content <- map words . lines <$> readFile (head args)
  when (checkDependsOn content) $ handleError InvalidInput

  -- Construct Graph
  let dependencyList   = extractDeps content
  let inputDepOrder    = map fst dependencyList
  let constructedGraph = foldr addDeps M.empty dependencyList

  -- cyclicCheck TODO

  -- Print result
  mapM_
    (\i -> printBack i $ S.toList $ makeDependenciesList constructedGraph i)
    inputDepOrder

-- | Error handler
handleError :: Error -> IO ()
handleError err = do
  putStrLn . fromJust $ lookup err errorList
  exitSuccess
 where
  errorList =
    [ (InvalidParam, "Usage: stack exec dep-lib <filename>")
    , (InvalidInput, "Error while parsing the file")
    , ( CircularDependency
      , "A dependency conflict present in the input (cyclic dependency)"
      )
    ]

-- | Print back final result
printBack :: String -> [String] -> IO ()
printBack parentDep childDep = do
  putStrLn $ parentDep ++ " depends on " ++ unwords childDep
