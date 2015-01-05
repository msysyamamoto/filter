{-# LANGUAGE OverloadedStrings #-}
module Tools.Filter where

import           Data.Set            (Set, fromList, notMember)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString.Char8 as B

import           Data.Monoid
import           Options.Applicative

data Option = Option
    { input   :: FileProp
    , pattern :: FileProp
    } deriving (Show)

data FileProp = FileProp
    { positions :: Int
    , delimiter :: Char
    , path      :: FilePath
    } deriving (Show)

positionsP :: String -> Parser Int
positionsP lavel = option auto
    (  long lavel
    <> metavar "POSITION"
    <> help "The list specifies fields"
    <> value 0
    <> showDefault
    )

delimiterP :: String -> Parser Char
delimiterP lavel = option auto
    (  long lavel
    <> metavar "DELIMITER"
    <> help "Field delimiter character."
    <> value '\t'
    <> showDefault
    )

filePropP :: String -> String -> Parser FileProp
filePropP lavel delimLavel = fp <$> positionsP lavel <*> delimiterP delimLavel
  where
    fp pos delim = FileProp pos delim ""

fileP :: String -> Parser FilePath
fileP label = strArgument
    (  metavar label
    <> help "file path"
    )

optionP :: Parser Option
optionP = (<*>) helper $ opts <$> filePropP "pos1" "delim1"
                              <*> filePropP "pos2" "delim2"
                              <*> fileP "filepath1"
                              <*> fileP "filepath2"
  where
    opts fp1 fp2 f1 f2 = Option fp1' fp2'
      where
        fp1' = fp1 { path = f1 }
        fp2' = fp2 { path = f2 }

filterNotMatching :: Option -> IO [String]
filterNotMatching (Option (FileProp pos1 delim1 f1) (FileProp pos2 delim2 f2)) = do
    ls   <- map (B.split delim1)      <$> readLines f1
    pats <- buildPatterns pos2 delim2 <$> readLines f2
    return . map (B.unpack . join delim1) $ runFilter pos1 pats ls

readLines :: FilePath -> IO [ByteString]
readLines filePath = B.lines <$> B.readFile filePath

buildPatterns :: Int -> Char -> [ByteString] -> Set ByteString
buildPatterns pos delim rows = fromList $ map (\x -> x !! pos) rows'
  where
    rows' = map (B.split delim) rows

runFilter :: Int -> Set ByteString -> [[ByteString]] -> [[ByteString]]
runFilter pos keys = filter isNotMatching
  where
    isNotMatching items = notMember (items !! pos) keys

join :: Char -> [ByteString] -> ByteString
join glue = go
  where
    glue' = B.singleton glue
    go []     = ""
    go [x]    = x
    go (x:xs) = B.append (B.append x glue') $ go xs
