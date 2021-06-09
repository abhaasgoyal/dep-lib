module Main where
import           Control.Monad                  ( when, unless )
import qualified Data.Map                      as M
import qualified Data.Map.Internal.Debug       as Md
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import qualified Data.Set                      as S
import           DepParser                      ( Graph
                                                , addDeps
                                                , cyclicCheck
                                                , makeDependenciesList
                                                )
import           FileHandlers                   ( checkInput
                                                , extractDepsFromFile
                                                , printOutput
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
-- | Error types
data Error =
    InvalidArgs -- ^ Invalid arguments
  | CircularDependency Graph -- ^ Input is not a DAG
  | InvalidInputFile -- ^ Error parsing input file
  deriving (Eq)

-- | Driver function
main :: IO ()
main = do
  file <- gArgs
  content <- readInput file
  let dependencyList   = extractDepsFromFile content
  let inputDepOrder    = map fst dependencyList
  let constructedGraph = foldr addDeps M.empty dependencyList
  acyclicCheck constructedGraph
  printResult inputDepOrder constructedGraph
  exitSuccess

-- | Get arguments
gArgs :: IO String
gArgs = do
  args <- getArgs
  when (length args /= 1) $ handleError InvalidArgs
  return $ head args

-- | Read file
readInput :: String -> IO [[String]]
readInput file = do
  content <- map words . lines <$> readFile file
  unless (checkInput content) $ handleError InvalidInputFile
  return content

-- | Checking whether constructed graph is acyclic
acyclicCheck :: Graph -> IO()
acyclicCheck graph = do
  let potentialCycles  = cyclicCheck graph
  when (isJust potentialCycles)
    $ handleError (CircularDependency $ fromJust potentialCycles)

-- | Print result
printResult :: [String] -> Graph -> IO()
printResult inputDepOrder graph =   mapM_
    (\i -> printOutput i . S.toList $ makeDependenciesList graph i)
    inputDepOrder

-- | Error handler
handleError :: Error -> IO ()
handleError err = do
  putStrLn $ case err of
    InvalidArgs      -> "Usage: stack exec dep-lib <filename>"
    InvalidInputFile -> "Error while parsing the file"
    CircularDependency graph ->
      "A cyclic dependency conflict present.\nDebugger found the following circular graph\n\n"
        ++ Md.showTreeWith (\k x -> show (k, S.toList x)) True True graph
  exitFailure
