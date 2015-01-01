{-# LANGUAGE OverloadedStrings #-}
module Tools.Filter where

import           Control.Applicative ((<$>))
import           Data.Set            (Set, fromList, notMember)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString.Char8 as B

filterNotMatching :: FilePath -> FilePath -> IO [String]
filterNotMatching f1 f2 = do
    ls   <- map (B.split '\t') <$> readLines f1
    keys <- fromList <$> readLines f2
    return . map (B.unpack . join "\t") $ runFilter ls keys

readLines :: FilePath -> IO [ByteString]
readLines path = B.lines <$> B.readFile path

runFilter :: [[ByteString]] -> Set ByteString -> [[ByteString]]
runFilter lines keys = filter (isNotMatching keys) lines

isNotMatching :: Set ByteString -> [ByteString] -> Bool
isNotMatching keys (key:_) = notMember key keys
isNotMatching _    _       = False

join :: ByteString -> [ByteString] -> ByteString
join glue = go
  where
    go []     = ""
    go [x]    = x
    go (x:xs) = B.append (B.append x glue) $ go xs

printUsage :: String -> String -> IO ()
printUsage pgname err = do
    putStrLn $ "Error: " ++ err
    putStrLn $ "Usage: " ++ pgname ++ " file1 file2"

parseArgs :: [String] -> Either String (FilePath, FilePath)
parseArgs args
    | length args < 2  = Left "missing argument"
    | otherwise = Right (args !! 0, args !! 1)
