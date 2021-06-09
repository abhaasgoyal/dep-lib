module Main where
import           Control.Monad                  ( when )
import qualified Data.Map                      as M
                                                ( empty )
import qualified Data.Map.Internal.Debug       as Md
                                                ( showTreeWith )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import qualified Data.Set                      as S
                                                ( toList )
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
  -- Get arguments
  args <- getArgs
  when (length args /= 1) $ handleError InvalidArgs

  -- Read file
  content <- map words . lines <$> readFile (head args)
  when (checkInput content) $ handleError InvalidInputFile

  -- Parse file to create intermediate Graph representation
  let dependencyList   = extractDepsFromFile content
  let inputDepOrder    = map fst dependencyList
  let constructedGraph = foldr addDeps M.empty dependencyList

  -- Checking whether constructed graph is acyclic
  let potentialCycles  = cyclicCheck constructedGraph
  when (isJust potentialCycles)
    $ handleError (CircularDependency $ fromJust potentialCycles)

  -- Print result
  mapM_
    (\i -> printOutput i . S.toList $ makeDependenciesList constructedGraph i)
    inputDepOrder

  exitSuccess

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
