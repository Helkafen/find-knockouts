{-# LANGUAGE NoImplicitPrelude #-}

module Vcf (readVcfWithGenotypes, writeVariants) where

import Protolude
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Types
import Utils (readGzippedLines)
import Parsers (parseVariant)

sampleIdsInHeader :: Text -> V.Vector SampleId
sampleIdsInHeader header = V.fromList $ map SampleId $ drop 9 (T.splitOn "\t" header)

parseVcfContent :: [Text] -> Either Error [RealVariant]
parseVcfContent lines = case lines of
    [] -> Left $ ParsingError "Empty vcf file"
    (header:rest) -> pure $ rights $ map (parseVariant sampleIdentifiers) rest -- TODO: exception for a left
        where sampleIdentifiers = sampleIdsInHeader header

readVcfWithGenotypes :: FilePath -> IO (Either Error [RealVariant])
readVcfWithGenotypes path = parseVcfContent <$> readGzippedLines path

tsvHeader :: V.Vector SampleId -> Text
tsvHeader samples = T.intercalate "\t" $ ["#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT"] <> map toStr (V.toList samples)
  where toStr (SampleId i) = i

variantToTsv :: KOVariant -> Text
variantToTsv (KOVariant (Variant (Chromosome c) (Position p) (GeneName n) (BaseSequence ref) (BaseSequence alt) geno _) homozygousKOPositions heterozygousKOPositions) =
  T.intercalate "\t" $ [c, show p, n, ref, alt, "", "PASS", info, "GT"] <> map genToS (V.toList geno)
  where genToS Geno00 = "0|0"
        genToS Geno01 = "0|1"
        genToS Geno10 = "1|0"
        genToS Geno11 = "1|1"
        info =  (T.intercalate ";" (filter ((>0) . T.length) [homStr, hetStr]))
        homStr = case homozygousCounts of
          [] -> ""
          xs -> "HOM=" <> (T.intercalate "," (map formatPositionCount homozygousCounts))
        hetStr = case heterogygousCounts of
          [] -> ""
          xs -> "HET=" <> (T.intercalate "," (map formatPositionCount heterogygousCounts))
        homozygousCounts =  M.toList $ countOccurs $ mconcat (M.elems homozygousKOPositions) :: [(Position, Int)]
        heterogygousCounts =  reverse $ sortBy (comparing snd) $ M.toList $ countOccurs $ mconcat (M.elems heterozygousKOPositions) :: [(Position, Int)]
        formatPositionCount ((Position p), count) = show p <> T.pack "x" <> show count

writeVariants :: FilePath -> [KOVariant] -> IO ()
writeVariants outFile [] = B.writeFile outFile (B.fromStrict (encodeUtf8 (tsvHeader V.empty)))
writeVariants outFile variants@((KOVariant v _ _):_) = B.writeFile outFile (B.fromStrict (encodeUtf8 formattedKnockouts))
  where formattedKnockouts = T.intercalate "\n" (tsvHeader samples:map variantToTsv variants)
        samples = sampleIds v


countOccurs :: (Eq k, Ord k) => [k] -> M.Map k Int
countOccurs l = foldr (\k d ->  M.insertWith (+) k 1 d) M.empty l