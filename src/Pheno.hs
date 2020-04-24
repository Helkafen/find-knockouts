{-# LANGUAGE DeriveGeneric, PartialTypeSignatures #-}

module Pheno where

import           Data.Text    (Text)
import qualified Data.Vector as V
import           GHC.Generics
import           Data.Csv
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           Data.Map.Merge.Lazy (merge)
import qualified Data.Map as M

data Foo
    = Foo
    { foo :: Text
    , bar :: Int
    } deriving (Eq, Show, Generic)
instance FromNamedRecord Foo

data Phenos = Phenos {
    phenosSubjectId :: Text,
    pheno1 :: Float
} deriving (Eq, Show, Generic)

data LinkerRecord = LinkerRecord {
    linkerSubjectId :: Text,
    linkerTopmedId :: Text
} deriving (Eq, Show, Generic)

data Covariates = Covariates {
    covarSubjectId :: Text,
    covAge :: Int,
    covSmoker :: Bool
} deriving (Eq, Show, Generic)

data FullRecord = FullRecord {
    topmedId :: Text,
    covariates :: Covariates,
    phenos :: Phenos
} deriving (Eq, Show, Generic)

instance FromNamedRecord LinkerRecord where
    parseNamedRecord m = LinkerRecord <$> m .: "SOURCE_SUBJECT_ID" <*> m .: "TOPMED_ID"

decodeByNameWithSpaces = decodeByNameWith (defaultDecodeOptions {decDelimiter = fromIntegral (ord ' ')})

decodeLinker :: BL.ByteString -> Either String (Header, V.Vector LinkerRecord)
decodeLinker = decodeByNameWithSpaces

mkDict :: Ord b => (a -> b) -> V.Vector a -> M.Map b a
mkDict f v = M.fromList $ map (\v -> (f v, v)) (V.toList v)

join :: V.Vector Phenos -> V.Vector LinkerRecord -> V.Vector Covariates -> M.Map Text FullRecord
join phenos links covariates = M.intersectionWithKey (\tid (pheno, link) cov -> FullRecord tid cov pheno) joined1 (mkDict covarSubjectId covariates)
    where joined1 :: M.Map Text (Phenos, LinkerRecord)
          joined1 = M.intersectionWithKey (\tid pheno link -> (pheno, link)) (mkDict phenosSubjectId phenos) (mkDict linkerSubjectId links)

i :: IO ()
i = do
    --x <- readFile "source/Phenotypes/Release5b/JHS_PhenoFile_wLinker.txt"
    let csv = "dbGaP_Subject_ID SUBJECT_ID SUBJECT_SOURCE SOURCE_SUBJECT_ID TOPMED_ID\n345698 306 JHS_CARe PT-7ZZX NWD100014\n346073 2905 JHS_CARe PT-7YKZ NWD100597"
    print $ decodeLinker csv