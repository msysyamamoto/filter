import System.Environment (getArgs, getProgName)
import Control.Applicative ((<$>))
import Tools.Filter

main :: IO ()
main = do
    pgname <- getProgName
    args   <- parseArgs <$> getArgs
    case args of
        Left err             -> printUsage pgname err
        Right (file1, file2) -> do
            filtered <- filterNotMatching file1 file2
            mapM_ putStrLn filtered
