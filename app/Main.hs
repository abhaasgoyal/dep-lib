module Main where
import           Control.Monad
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Set                      as S
import           DepParser
import           System.Environment
import           System.Exit

-- | Error types
data Error =
    InvalidArgs -- ^ Invalid arguments
  | CircularDependency -- ^ Input is not a DAG
  | InvalidInputFile -- ^ Error parsing input file
  deriving (Eq)

-- | Driver function
main :: IO ()
main = do
  -- Get arguments
  args <- getArgs
  when (length args /= 1) $ handleError InvalidArgs

  -- Read file
  content <- map words . lines <$> readFile (head args)
  when (checkDependsOn content) $ handleError InvalidInputFile

  -- Construct Graph
  let dependencyList   = extractDeps content
  let inputDepOrder    = map fst dependencyList
  let constructedGraph = foldr addDeps M.empty dependencyList

  -- Checking whether input is acyclic
  when (cyclicCheck constructedGraph) $ handleError CircularDependency

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
    [ (InvalidArgs       , "Usage: stack exec dep-lib <filename>")
    , (InvalidInputFile  , "Error while parsing the file")
    , (CircularDependency, "A cyclic dependency conflict present in the input")
    ]

-- | Print back final result
printBack :: String -> [String] -> IO ()
printBack parentDep childDep = do
  putStrLn $ parentDep ++ " depends on " ++ unwords childDep
