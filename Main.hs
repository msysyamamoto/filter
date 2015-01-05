import Data.Monoid
import Options.Applicative
import Tools.Filter

main :: IO ()
main = do
    opts     <- execParser (info optionP mempty)
    filtered <- filterNotMatching opts
    mapM_ putStrLn filtered
