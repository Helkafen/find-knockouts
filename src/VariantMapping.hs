module VariantMapping (readVariantMapping) where

import Protolude
import qualified Data.Map as M
import qualified Data.Text as T

import Types
import Utils (maybeReadFile)

parseVariantMapping :: Text -> M.Map SystematicVariantName GeneName
parseVariantMapping = M.fromList . mapMaybe parseL . T.lines
  where parseL line = case T.words line of
          [variant, gene] -> Just (SystematicVariantName variant, GeneName gene)
          _ -> Nothing

readVariantMapping :: FilePath -> IO (Maybe (M.Map SystematicVariantName GeneName))
readVariantMapping path = fmap parseVariantMapping <$> maybeReadFile path