module Main where

import System.Environment
import System.Exit
import Data.Functor
import System.IO
import Options.Applicative
import Options.Applicative.Types
import Control.Monad


import Lib

data Opts = Opts
  { inpFile :: IO Handle
  , outFile :: IO Handle
  , precise :: Int
  }

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
versionOption = infoOption "0.1.0.0"  (long "version"
                                    <> help "Show version")

optsParser :: ParserInfo Opts
optsParser = info (helper <*> versionOption <*> programOptions)
                  (fullDesc <> progDesc "Quick stats Widget" <> header "stats - a widget for computing average, stddev, and variance")

printStats :: Handle -> Int -> [Int] -> IO ()
printStats h n = do hPutStrLn h . showAve     n
                    hPutStrLn h . showStdDev  n
                    hPutStrLn h . showVar     n

work :: Opts -> IO ()
work opt = do
              i <-    inpFile opt
              o <-    outFile opt
              let p = precise opt
              hGetContents i >>= printStats o p . fmap (read :: String -> Int) . lines
main :: IO ()
main = execParser optsParser >>= work
