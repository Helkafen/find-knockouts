module Main where
  
import Prelude
import Test.Tasty.Discover
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe (isNothing, mapMaybe)

import Types
import Knockout
import Data.List (transpose)

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

  
main :: IO ()
main = defaultMain tests
  
tests :: TestTree
tests = testGroup "Tests" [
    QC.testProperty "prop_empty_extract_knockouts" prop_empty_extract_knockouts,
    QC.testProperty "prop_extract_homozygote_knockout" prop_extract_homozygote_knockout,
    QC.testProperty "prop_ignore_unknown_genes" prop_ignore_unknown_genes,
    QC.testProperty "prop_extract_compound_heterozygote" prop_extract_compound_heterozygote,
    QC.testProperty "prop_ignore_non_knockout" prop_ignore_non_knockout,
    QC.testProperty "prop_break_by_gene" prop_break_by_gene,
    QC.testProperty "prop_replaceByKOVariant_null" prop_replaceByKOVariant_null,
    QC.testProperty "prop_replaceByKOVariant_homozygotes_are_KOs" prop_replaceByKOVariant_homozygotes_are_KOs,
    QC.testProperty "prop_replaceByKOVariant_KOs_are_shuffled_like_genotypes" prop_replaceByKOVariant_KOs_are_shuffled_like_genotypes
    ]
-- {-# OPTIONS_GHC -F -pgmF tasty-discover #-}



var p g s = Variant (Chromosome "1") (Position p) Nothing (BaseSequence "A") (BaseSequence "T") (V.fromList g) (V.fromList s)
--var2 p = Variant (Chromosome "1") (Position p) Nothing (BaseSequence "A") (BaseSequence "T")

prop_empty_extract_knockouts = null $ extractKnockouts emptyGeneNames []
  where emptyGeneNames = M.fromList []

prop_extract_homozygote_knockout = extractKnockouts geneNameOfVariant [variant] == [KOVariant (variant {genotypes = V.fromList [Geno01, Geno00], variantId = gene1}) (M.singleton (SampleId "NWD1") [Position 10]) (M.empty)]
  where variant = var 10 [Geno11, Geno00] [SampleId "NWD1", SampleId "NWD2"]
        geneNameOfVariant = M.fromList [(systematicVariantName variant, gene1)]
        gene1 = GeneName "GENE1"

prop_ignore_unknown_genes = null $ extractKnockouts geneNameOfVariant [variant]
  where variant = var 10 [Geno11, Geno00] [SampleId "NWD1", SampleId "NWD2"]
        geneNameOfVariant = M.fromList []

prop_extract_compound_heterozygote = extractKnockouts geneNameOfVariant [variant1, variant2] == [KOVariant (variant1 {genotypes = V.fromList [Geno01, Geno01], variantId = gene1}) M.empty (M.fromList [(SampleId "NWD1", [Position 10, Position 11]),(SampleId "NWD2", [Position 10, Position 11])])]
  where variant1 = var 10 [Geno01, Geno10] [SampleId "NWD1", SampleId "NWD2"]
        variant2 = var 11 [Geno10, Geno01] [SampleId "NWD1", SampleId "NWD2"]
        geneNameOfVariant = M.fromList [(systematicVariantName variant1, gene1), (systematicVariantName variant2, gene1)]
        gene1 = GeneName "GENE1"

prop_ignore_non_knockout = null $ extractKnockouts geneNameOfVariant [variant1, variant2]
  where variant1 = var 10 [Geno00, Geno00, Geno01, Geno10] [SampleId "NWD1", SampleId "NWD2", SampleId "NWD3", SampleId "NWD4"]
        variant2 = var 11 [Geno10, Geno01, Geno01, Geno10] [SampleId "NWD1", SampleId "NWD2", SampleId "NWD3", SampleId "NWD4"]
        geneNameOfVariant = M.fromList $ map (\v -> (systematicVariantName v, GeneName "GENE1")) [variant1, variant2]

prop_break_by_gene = null $ extractKnockouts geneNameOfVariant [variant1, variant2]
  where variant1 = var 10 [Geno01, Geno10] [SampleId "NWD1", SampleId "NWD2"]
        variant2 = var 11 [Geno10, Geno01] [SampleId "NWD1", SampleId "NWD2"]
        geneNameOfVariant = M.fromList [(systematicVariantName variant1, GeneName "GENE1"), (systematicVariantName variant2, GeneName "GENE2")]

prop_replaceByKOVariant_null = isNothing (replaceGenotypeByKOVariant [])

-- Variants [
-- Variant {chromosome = Chromosome "1", position = Position 10, variantId = Nothing, reference = BaseSequence "A", alternative = BaseSequence "T", genotypes = [Geno10,Geno10], sampleIds = [SampleId "NWD1",SampleId "NWD2"]},
-- Variant {chromosome = Chromosome "1", position = Position 10, variantId = Nothing, reference = BaseSequence "A", alternative = BaseSequence "T", genotypes = [Geno11,Geno11], sampleIds = [SampleId "NWD1",SampleId "NWD2"]}]

-- Pour un gene:
--  si il n'y a pas de KO, alors personne n'était homozygote pour aucun variant
--  si il y a un KO, alors le nombre de personnes qui sont KO est supérieur ou égal au nombre de personnes qui sont homozygotes sur au moins un variant


prop_replaceByKOVariant_homozygotes_are_KOs (VariantsOfOneGene variants) = case replaceGenotypeByKOVariant annotatedVariants of
                                                                             Nothing -> numberOfPeopleWithHomo == 0
                                                                             Just (KOVariant v _ _) -> length (V.filter (==Geno01) (genotypes v)) >= numberOfPeopleWithHomo
  where personHasHomoList =  map (Geno11 `elem`) $ transpose (map (V.toList . genotypes) annotatedVariants)
        numberOfPeopleWithHomo = length $ filter (==True) personHasHomoList
        annotatedVariants = mapMaybe (annotateVariant geneNameOfVariant) variants
        geneNameOfVariant = case variants of
          (variant1:_) -> M.fromList (map (\v -> (systematicVariantName v, GeneName "GENE1")) variants)
          [] -> M.fromList []



prop_replaceByKOVariant_KOs_are_shuffled_like_genotypes (VariantsOfOneGene variants) = koNormal == koDoubleReverse
  where koNormal = genotypesOfKoVariants <$> replaceGenotypeByKOVariant annotatedVariants
        koDoubleReverse = V.reverse . genotypesOfKoVariants <$> replaceGenotypeByKOVariant reversedVariants
        annotatedVariants = mapMaybe (annotateVariant geneNameOfVariant) variants
        reversedVariants = map reverseGenotypes annotatedVariants
        reverseGenotypes v = v { genotypes = V.reverse (genotypes v) }
        genotypesOfKoVariants (KOVariant v _ _) = genotypes v
        geneNameOfVariant = case variants of
          (variant1:_) -> M.fromList (map (\v -> (systematicVariantName v, GeneName "GENE1")) variants)
          [] -> M.fromList []
