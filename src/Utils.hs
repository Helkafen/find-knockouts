{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Protolude hiding (decodeUtf8With)
import qualified Data.Text.Lazy as TL
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding (decodeUtf8With)

import Types

readGzippedLines :: FilePath -> IO [Text]
readGzippedLines path = map TL.toStrict . TL.lines . decodeUtf8With ignore . GZip.decompress <$> B.readFile path



maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile path = do
  contentEither <- try (readFile path) :: IO (Either SomeException Text)
  case contentEither of 
    Left _ -> do putStrLn ("Could not read file " ++ path)
                 return Nothing
    Right content -> return (Just content)
