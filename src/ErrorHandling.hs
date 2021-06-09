-- |

module ErrorHandling (Error(..), handleError) where
import DepParser (Graph)
import qualified Data.Set as S
import qualified Data.Map.Internal.Debug       as Md
import           System.Exit                    ( exitFailure)

-- | Error types
data Error =
    InvalidArgs -- ^ Invalid arguments
  | CircularDependency Graph -- ^ Input is not a DAG
  | InvalidInputFile -- ^ Error parsing input file
  deriving (Eq, Show)

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
