-- Create a vcf file where the minor allele is 1 iif there is a knockout. Input must only contain LoF variants
module Main where

import System.Environment (getArgs)
import Knockout (run)

main :: IO()
main = do
  args <- getArgs
  case args of
    [geneNamesFile, inFile, outFile] -> run geneNamesFile inFile outFile
    _ -> putStrLn ("Usage: gene_names.tab input.vcf.gz output.vcf")

