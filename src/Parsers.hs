{-# LANGUAGE NoImplicitPrelude #-}

module Parsers where

import Protolude
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Either.Unwrap (mapLeft)

import Types

chromosomeParser :: Parser Chromosome
chromosomeParser = choice [autosomeParser, xParser, yParser]
  where autosomeParser = do
          d <- decimal
          guard $ d > 0 && d < 23
          pure (Chromosome $ show (d :: Integer))
        xParser = string "X" $> Chromosome "X"
        yParser = string "Y" $> Chromosome "Y"

genoParser :: Parser Genotype
genoParser = choice [
  string "0|0" $> Geno00,
  string "0|1" $> Geno01,
  string "1|0" $> Geno10,
  string "1|1" $> Geno11]

variantIdParser :: Parser (Maybe Text)
variantIdParser = choice [none, rs] <?> "variant_name"
  where rs = (Just . T.pack) <$> many1 (notChar '\t')
        none = char '.' $> Nothing

-- Example:
-- -- #CHROM    POS    ID    REF    ALT    QUAL    FILTER    INFO    FORMAT
-- -- 21    13620225    .    GT    G    64    PASS    AC=1;AN=108070    GT    0|0
variantParser :: V.Vector SampleId -> Parser RealVariant
variantParser sampleIdentifiers = do
  c <- chromosomeParser <* tab
  pos <- Position <$> decimal <* tab
  name <- variantIdParser <* tab
  ref <- (BaseSequence . T.pack)<$> many1 letter <* tab
  alt <- (BaseSequence . T.pack) <$> many1 letter <* tab
  skipField >> skipField >> skipField >> skipField
  geno <- V.fromList <$> genoParser `sepBy` char '\t'
  guard $ length geno == length sampleIdentifiers
  return $ Variant c pos name ref alt geno sampleIdentifiers
  where
    skipField = skipWhile (/= '\t') >> skip (== '\t')
    tab = skip (== '\t')

parseVariant :: V.Vector SampleId -> Text -> Either Error RealVariant
parseVariant sampleIdentifiers s = mapLeft (ParsingError . (\e -> s <> " " <> T.pack e)) (parseOnly (variantParser sampleIdentifiers) s)
