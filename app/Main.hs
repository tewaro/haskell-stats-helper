module Main where

import System.Environment
import System.Exit
import Data.Functor
import System.IO
import Options.Applicative
import Options.Applicative.Types
import Control.Monad
import Data.Function


import Lib

data Opts = Opts
  { inpFile :: IO Handle
  , outFile :: IO Handle
  , precise :: Int
  }

(.>) = flip (.)

handleAsk :: IOMode -> ReadM (IO Handle)
handleAsk m =  (`openFile` m) <$> readerAsk

programOptions :: Parser Opts
programOptions =  Opts <$>
          option (handleAsk ReadMode)   (long "input"
                                      <> short 'i'
                                      <> metavar "FILEPATH"
                                      <> help "Input File"
                                      <> value (return stdin :: IO Handle))
      <*> option (handleAsk WriteMode)  (long "output"
                                      <> short 'o'
                                      <> metavar "FILEPATH"
                                      <> help "Output File"
                                      <> value (return stdout :: IO Handle))
      <*> option  auto                  (long "precision"
                                      <> short 'p'
                                      <> metavar "NUMBER"
                                      <> help "Decimal Precision"
                                      <> value 10
                                      <> showDefault)

versionOption :: Parser (a -> a)
versionOption = infoOption "v0.1.0.0"  (long "version"
                                     <> help "Show version")

optsParser :: ParserInfo Opts
optsParser = info (helper <*> versionOption <*> programOptions)
                  (fullDesc <> progDesc "Quick stats Widget"
                            <> header "stats - a widget for computing average, stddev, and variance")

printStats :: Handle -> Int -> [Int] -> IO ()
printStats h n xs = do
                      hPutStrLn h $ showAve     n xs
                      hPutStrLn h $ showStdDev  n xs
                      hPutStrLn h $ showVar     n xs

work :: Int -> Handle -> Handle -> IO ()
work p i o = hGetContents i >>= lines .> fmap (read :: String -> Int) .> printStats o p

main :: IO ()
main = execParser optsParser >>= \opt -> work (precise opt) <$> inpFile opt <*> outFile opt & join
