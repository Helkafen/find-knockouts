{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Protolude
import Test.QuickCheck
import Data.List (zip3)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M

newtype Chromosome = Chromosome Text
  deriving (Eq, Show)

newtype Position = Position Int
  deriving (Eq, Ord, Show)

data Genotype = Geno00 | Geno01 | Geno10 | Geno11
  deriving (Eq, Show)

data Error = ParsingError Text | MissingFile FilePath | NoVariantFound FilePath
  deriving (Show)

-- The ID of a person
newtype SampleId = SampleId Text
  deriving (Eq, Ord, Show)

newtype BaseSequence = BaseSequence Text
  deriving (Eq, Show)

-- A variant name created from its chromosome and position. All variants have one
newtype SystematicVariantName = SystematicVariantName Text
  deriving (Eq, Ord, Show)

newtype GeneName = GeneName Text
  deriving (Eq, Show)

data Variant a = Variant {
  chromosome :: Chromosome,
  position :: Position,
  variantId :: a,
  reference :: BaseSequence,
  alternative :: BaseSequence,
  genotypes :: V.Vector Genotype,
  sampleIds :: V.Vector SampleId
} deriving (Eq, Show)

type RealVariant = Variant (Maybe Text)

type AnnotatedVariant = Variant GeneName

--data Heterozygosity = Homozygous | Heterozygous
--  deriving (Eq, Show)

data KOVariant = KOVariant {
  koVar :: (Variant GeneName),
  koHomozygousKOPositions :: M.Map SampleId [Position],
  koHeterozygousLofPositions :: M.Map SampleId [Position]
} deriving (Eq, Show)

instance Arbitrary Genotype where
  arbitrary = elements [Geno00, Geno01, Geno10, Geno11]

newtype VariantsOfOneGene = VariantsOfOneGene [RealVariant]
  deriving (Eq, Show)

instance Arbitrary VariantsOfOneGene where
  arbitrary = do
    NonNegative sampleNumber <- arbitrary
    NonNegative variantNumber <- arbitrary
    allGenotypes <- vectorOf variantNumber (V.fromList <$> vectorOf sampleNumber arbitrary) :: Gen [V.Vector Genotype]
    let sampleIdentifiers = V.fromList $ map (\n -> SampleId (T.pack $ "NWD" ++ show n)) [1..sampleNumber]
    let variants = map (\variantGenotype -> Variant (Chromosome "1") (Position 10) Nothing (BaseSequence "A") (BaseSequence "T") variantGenotype sampleIdentifiers) allGenotypes
    pure $ VariantsOfOneGene variants
