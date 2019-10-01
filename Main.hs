{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

-- Put everything in one file for now

module Main where

import Codec.Picture
import Codec.Picture.Types
import Linear.Quaternion
import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { size :: (Int,Int)  -- size of output image in pixels
  , inp  :: String 
  , out  :: String 
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = do
  size <- option auto
    (long "size" <> value (256,256) <> metavar "width,height" <>
     help "width and height of output in pixels")
  inp <- argument str (metavar "INPUT")
  out <- argument str (metavar "OUTPUT")
  pure Options {..}

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "resampling of images between two projections")
main = do
   options <- execParser opts
   print options
