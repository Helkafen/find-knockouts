{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Knockout where

import Protolude
import qualified Data.Text as T
import Data.Text.Encoding.Error (ignore)
import Control.Error.Util (hoistEither)
import Control.Monad.Except (withExceptT)
import Control.Exception (try)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List (groupBy, zip3)

import Types
import Vcf (readVcfWithGenotypes, writeVariants)
import Utils (maybeReadFile)
import VariantMapping (readVariantMapping)

isHeterogyzote :: Genotype -> Bool
isHeterogyzote Geno00 = False
isHeterogyzote Geno11 = False
isHeterogyzote _ = True

replaceGenotypeByKOVariant :: [AnnotatedVariant] -> Maybe KOVariant
replaceGenotypeByKOVariant [] = Nothing
replaceGenotypeByKOVariant variants@(v:_) = if Geno01 `elem` koGenotypes then Just (KOVariant (v { genotypes = koGenotypes }) homozygousKOPositions heterozygousKOPositions) else Nothing
  where genotypesOfGene = transpose $ map (V.toList . genotypes) variants :: [[Genotype]]
        koGenotypes = V.fromList $ map (\genotypes -> if personIsKO genotypes then Geno01 else Geno00) genotypesOfGene
        --heterozygousLofPositions = [(s,p) | (p,h,s) <- zip3 (map position variants) (map isHeterogyzote genotypesOfGene) (V.toList (sampleIds v)), h]
        --homozygousKOPositions = [(s,p) | (p,h,s) <- zip3 (map position variants) (map (== Geno11) genotypesOfGene) (V.toList (sampleIds v)), h]
        homozygousKOPositions = (homoPositionsOfVariants variants) :: Map SampleId [Position]
        heterozygousKOPositions = (heteroPositionsOfVariants variants) :: Map SampleId [Position]
        --x = zip
        personIsKO genotypes = lofRight && lofLeft
          where lofRight = any (`elem` [Geno01, Geno11]) genotypes
                lofLeft = any (`elem` [Geno10, Geno11]) genotypes

homoPositionsOfVariant :: AnnotatedVariant -> M.Map SampleId [Position]
homoPositionsOfVariant variant = M.fromList [(s, [pos]) | (s,g) <- zip samples gen, g == Geno11]
  where gen = V.toList (genotypes variant) :: [Genotype]
        samples = V.toList (sampleIds variant) :: [SampleId]
        pos = position variant :: Position

heteroPositionsOfVariant :: AnnotatedVariant -> M.Map SampleId [Position]
heteroPositionsOfVariant variant = M.fromList [(s, [pos]) | (s,g) <- zip samples gen, g `elem` [Geno01, Geno10]]
  where gen = V.toList (genotypes variant) :: [Genotype]
        samples = V.toList (sampleIds variant) :: [SampleId]
        pos = position variant :: Position

homoPositionsOfVariants :: [AnnotatedVariant] -> M.Map SampleId [Position]
homoPositionsOfVariants = (M.unionsWith (<>)) . map homoPositionsOfVariant

heteroPositionsOfVariants :: [AnnotatedVariant] -> M.Map SampleId [Position]
heteroPositionsOfVariants = (M.unionsWith (<>)) . map heteroPositionsOfVariant



extractKnockouts :: M.Map SystematicVariantName GeneName -> [RealVariant] -> [KOVariant]
extractKnockouts geneNameOfVariant variants = mapMaybe replaceGenotypeByKOVariant variantsGroupedByGene
  where variantsGroupedByGene = groupBy (\a b -> variantId a == variantId b) (mapMaybe (annotateVariant geneNameOfVariant) variants)

annotateVariant :: M.Map SystematicVariantName GeneName -> RealVariant -> Maybe AnnotatedVariant
annotateVariant geneNameOfVariant v = case M.lookup (systematicVariantName v) geneNameOfVariant of
                                        Just n -> Just v {variantId = n }
                                        Nothing -> Nothing

systematicVariantName :: RealVariant -> SystematicVariantName
systematicVariantName v = SystematicVariantName $ "var_chr" <> c <> "_" <> show (p :: Int)
  where Chromosome c = chromosome v
        Position p = position v

run :: FilePath -> FilePath -> FilePath -> IO ()
run geneNamesFile inFile outFile = do
  loaded <- readVariantMapping geneNamesFile
  case loaded of
    Nothing -> die "The variant mapping file could not be loaded"
    Just variantToGene -> do
      putStrLn ("Loaded " ++ show (M.size variantToGene) ++ " variants with their gene name")
      variantsEither <- readVcfWithGenotypes inFile
      case variantsEither of
        Left e -> die "An error occured when reading the vcf file"
        Right variants -> do
          let knockouts = extractKnockouts variantToGene variants
          writeVariants outFile knockouts
