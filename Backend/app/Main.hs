module Main where

 import Options.Applicative
 import Utils.WriteToFiles

 type Opts = FilePath

 opts :: ParserInfo Opts
 opts =
  let parseOpts = argument str . mconcat $ [metavar "FILE", help "File to which the plutus script will be written"]
   in info (parseOpts <**> helper) . mconcat $ [fullDesc, progDesc "Create a forward minting policy for NFTs"]

 main :: IO ()
 main = execParser opts >>= writeMintingValidatorPlutusFile