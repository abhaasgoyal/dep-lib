module Main where
import           Control.Monad                  ( when )
import qualified Data.Map                      as M
                                                ( empty )
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as S
                                                ( toList )
import           DepParser                      ( addDeps
                                                , cyclicCheck
                                                , makeDependenciesList
                                                )
import           FileHandlers                   ( checkInput
                                                , extractDepsFromFile
                                                , printOutput
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitSuccess )

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
  when (checkInput content) $ handleError InvalidInputFile

  -- Parse file to create intermediate Graph representation
  let dependencyList   = extractDepsFromFile content
  let inputDepOrder    = map fst dependencyList
  let constructedGraph = foldr addDeps M.empty dependencyList

  -- Checking whether constructed graph is acyclic
  when (cyclicCheck constructedGraph) $ handleError CircularDependency

  -- Print result
  mapM_
    (\i -> printOutput i $ S.toList $ makeDependenciesList constructedGraph i)
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
